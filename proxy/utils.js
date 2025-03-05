import net from "net";
// Find an available port
export function getPort() {
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

export function wait(ms = 1000) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve("timeout");
    }, ms);
  });
}