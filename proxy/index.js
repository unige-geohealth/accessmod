// Import required modules
import { createServer } from "http";
import { Server } from "socket.io";
import { readFile } from "fs/promises";
import { fileURLToPath } from "url";
import express from "express";
import path from "path";
import { Session, sessions } from "./session.js";
import { ProgrammableProxy } from "./proxy.js";

const proxy = new ProgrammableProxy();

// Increase default max listeners

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

// Serve static files from current directory
app.use(express.static(__dirname));
app.use(proxy.middleware());

// Root route serves the container page
app.get("/", async (req, res) => {
  try {
    const content = await readFile(INDEX_HTML, "utf8");
    res.send(content);
  } catch (err) {
    res.status(500).send("Error loading index.html");
  }
});

// Dynamic proxy middleware for Shiny apps
// Note: We don't need this custom middleware as the main proxy middleware will handle all routes
// based on the registered paths
app.use("/app/:sessionId", (req, res, next) => {
  const sessionId = req.params.sessionId;
  const session = sessions.get(sessionId);

  if (!session) {
    return res.status(404).send("Session not found");
  }
  
  // Continue to the next middleware (which includes the proxy middleware)
  next();
});

// Socket.IO connection handling
io.on("connection", async (socket) => {
  try {
    let session = new Session(socket, proxy, ENTRYPOINT);
    await session.init();

    socket.on("message", async (value) => {
      if (value === "restart") {
        session.destroy();
        session = new Session(socket, proxy, ENTRYPOINT);
        await session.init();
      }
    });

    socket.on("disconnect", async () => {
      session.destroy();
    });
  } catch (error) {
    console.error(`Error handling socket connection:`, error);
    socket.emit("error", "Failed to initialize application");
  }
});

// Handle WebSocket upgrades
server.on('upgrade', (req, socket, head) => {
  proxy.handleUpgrade(req, socket, head);
});



// Start the server
server.listen(PROXY_PORT, () => {
  console.log(`Shiny Manager running at http://localhost:${PROXY_PORT}`);
});
