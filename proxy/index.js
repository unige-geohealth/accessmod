// Import required modules
import { createServer } from "http";
import { Server } from "socket.io";
import { readFile } from "fs/promises";
import { fileURLToPath } from "url";
import { dirname, join } from "path";
import express from "express";
import { randomUUID } from "crypto";
import { spawn } from "child_process";
import path from "path";
import net from "net";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const PROXY_PORT = 8080;
const ENTRYPOINT = path.join(__dirname, "demo", "app.R");
const INDEX_HTML = path.join(__dirname, "index.html");

// Initialize Express app
const app = express();
const server = createServer(app);
const io = new Server(server);
const sessions = new Map(); // Map of session_id -> {port, process, lastAccess}

// Serve static files from current directory
app.use(express.static(__dirname));

app.get("/", async (req, res) => {
  try {
    const content = await readFile(INDEX_HTML, "utf8");
    res.send(content);
  } catch (err) {
    res.status(500).send("Error loading index.html");
  }
});

io.on("connection", async (socket) => {
  const id = randomUUID();
  const port = await getPort();
  await start(port, id, socket);

  socket.on("message", async (value) => {
    if (value === "restart") {
      await start(port, id, socket);
    }
  });
});

async function start(port, id, socket) {
  const instance = sessions.get(id);
  const url = getUrl(port, id);
  if (instance?.process) {
    await instance.process.kill("SIGTERM");
  }
  const newInstance = await startProcess(port, id, url);
  if (!newInstance.ok) {
    throw new Error("Can't start");
  }
  socket.emit("init", url);
  sessions.set(id, newInstance);
}

async function startProcess(port, id, url) {
  return new Promise(async (resolve) => {
    const process = spawn("Rscript", [ENTRYPOINT, port], {
      cwd: path.dirname(ENTRYPOINT),
    });

    process.stdout.on("data", (data) => {
      console.log(`Shiny stdout [${id}]: ${data}`);
    });

    process.stderr.on("data", (data) => {
      console.log(`Shiny stderr [${id}]: ${data}`);
    });

    const ok = await tester(url);
    resolve({
      process,
      ok,
    });
  });
}

async function tester(url) {
  let ok = false;
  const max = 10;
  for (let i = 0; i < max; i++) {
    console.log(`Attempt ${i}/${max}`);
    await wait(1000);
    ok = await test(url);
    if (ok) {
      break;
    }
  }

  return ok;

  function test(url) {
    return new Promise(async (resolve) => {
      try {
        const u = new URL(url);
        u.path = "x/health";
        await fetch(u).then((r) => r.text());
        resolve(true);
      } catch (error) {
        resolve(false);
      }
    });
  }
}

// Start the server
server.listen(PROXY_PORT, () => {
  console.log(`Shiny Manager running at http://localhost:${PROXY_PORT}`);
});

function getUrl(port, id) {
  const url = `http://localhost:${port}/?sm_session_id=${id}`;
  return url;
}
// Find an available port
function getPort() {
  return new Promise((resolve) => {
    const server = net.createServer();
    server.listen(0, () => {
      const port = server.address().port;
      server.close(() => {
        console.log("PORT ", port);
        resolve(port);
      });
    });
  });
}

function wait(ms = 1000) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}
