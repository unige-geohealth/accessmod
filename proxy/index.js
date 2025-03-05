// Import required modules
import { createServer } from "http";
import { Server } from "socket.io";
import { readFile } from "fs/promises";
import { fileURLToPath } from "url";
import express from "express";
import path from "path";
import { EventEmitter } from "events";
import { Session, sessions } from "./session.js";

// Increase default max listeners
EventEmitter.defaultMaxListeners = 20;

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
app.use("/app/:sessionId", (req, res, next) => {
  const sessionId = req.params.sessionId;
  const session = sessions.get(sessionId);

  if (!session) {
    return res.status(404).send("Session not found");
  }
  // Use the proxy middleware
  session.proxy(req, res, next);
});


// Socket.IO connection handling
io.on("connection", async (socket) => {
  try {
    let session = new Session(socket, ENTRYPOINT);
    await session.init();

    socket.on("message", async (value) => {
      if (value === "restart") {
        session.destroy();
        session = new Session(socket, ENTRYPOINT);
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

// Handle websocket upgrade requests
server.on('upgrade', (req, socket, head) => {
  const pathname = new URL(req.url, 'http://localhost').pathname;
  const sessionMatch = pathname.match(/^\/app\/([^/]+)/);
  
  if (sessionMatch) {
    const sessionId = sessionMatch[1];
    const session = sessions.get(sessionId);
    
    if (session && session.proxy) {
      session.proxy.upgrade(req, socket, head);
    } else {
      socket.destroy();
    }
  }
});

// Start the server
server.listen(PROXY_PORT, () => {
  console.log(`Shiny Manager running at http://localhost:${PROXY_PORT}`);
});
