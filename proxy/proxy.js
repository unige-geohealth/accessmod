import httpProxy from "http-proxy";

export class ProgrammableProxy {
  constructor() {
    this.proxy = httpProxy.createProxyServer({
      ws: true,
    });
    this.routes = new Map();

    // Handle proxy errors
    this.proxy.on("error", (err, req, res) => {
      console.error("Proxy error:", err);
      if (res && res.writeHead) {
        res.writeHead(500, { "Content-Type": "text/plain" });
        res.end("Proxy error");
      }
    });

    this.middleware = this.middleware.bind(this);
    this.register = this.register.bind(this);
    this.unregister = this.unregister.bind(this);
    this.getRoutes = this.getRoutes.bind(this);
    this.handleUpgrade = this.handleUpgrade.bind(this);
  }

  /**
   * Register a new proxy route
   * @param {string} sourcePath - Source path to match (e.g., 'http://localhost:8080/app/c323do3i')
   * @param {string} targetUrl - Target URL to proxy to (e.g., 'http://localhost:3838')
   */
  register(sourcePath, targetUrl) {
    try {
      // Parse the source path to extract the path part
      const pathPattern = sourcePath;

      // Store the target URL for this path
      this.routes.set(pathPattern, targetUrl);

      return true;
    } catch (error) {
      console.error("Failed to register route:", error);
      return false;
    }
  }

  /**
   * Unregister a proxy route
   * @param {string} pathPattern - Path pattern to unregister
   */
  unregister(pathPattern) {
    try {
      // Use the path pattern directly, consistent with how register() works
      const result = this.routes.delete(pathPattern);

      return result;
    } catch (error) {
      console.error("Failed to unregister route:", error);
      return false;
    }
  }

  /**
   * Express middleware function
   */
  middleware() {
    return (req, res, next) => {
      // Check if we have a matching route
      let matched = false;
      let targetUrl = null;
      let matchedPattern = null;

      for (const [pathPattern, target] of this.routes.entries()) {
        if (req.path.startsWith(pathPattern)) {
          matched = true;
          targetUrl = target;
          matchedPattern = pathPattern;
          break;
        }
      }

      if (matched && targetUrl) {
        // Calculate the path relative to the matched pattern
        // This ensures we forward the request to the correct path on the target server
        const relativePath =
          req.originalUrl.substring(matchedPattern.length) || "/";

        // Preserve the original path when proxying
        const proxyOptions = {
          ws: true,
          target: targetUrl,
        };

        // Set the path for the proxied request
        // Make sure it starts with a slash but avoid double slashes
        req.url = relativePath.startsWith("/")
          ? relativePath
          : "/" + relativePath;

        // Forward the request to the target
        this.proxy.web(req, res, proxyOptions);
      } else {
        // No matching route, continue with next middleware
        next();
      }
    };
  }

  /**
   * Get all registered routes
   * @returns {Object} Map of routes
   */
  getRoutes() {
    return Object.fromEntries(this.routes);
  }

  /**
   * Handle WebSocket upgrade requests
   * @param {http.IncomingMessage} req - The request object
   * @param {net.Socket} socket - The network socket between the server and client
   * @param {Buffer} head - The first packet of the upgraded stream
   */
  handleUpgrade(req, socket, head) {
    // Check if we have a matching route
    let matched = false;
    let targetUrl = null;
    let matchedPattern = null;

    for (const [pathPattern, target] of this.routes.entries()) {
      if (req.url.startsWith(pathPattern)) {
        matched = true;
        targetUrl = target;
        matchedPattern = pathPattern;
        break;
      }
    }

    if (matched && targetUrl) {
      // Calculate the path relative to the matched pattern
      const relativePath = req.url.substring(matchedPattern.length) || "/";

      // Rewrite the URL to the relative path
      // Make sure it starts with a slash for consistency with middleware method
      req.url = relativePath.startsWith("/")
        ? relativePath
        : "/" + relativePath;

      // Proxy the WebSocket request
      this.proxy.ws(req, socket, head, { target: targetUrl });
    } else {
      socket.destroy();
    }
  }
}
