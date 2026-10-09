import { createServer } from "node:net";

/** A loopback TCP port that was free a moment ago, for an operations endpoint. */
export const freePort = async (): Promise<number> =>
  await new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      server.close(() =>
        typeof address === "object" && address !== null
          ? resolve(address.port)
          : reject(new Error("no port")),
      );
    });
  });

/** A loopback operations endpoint on a free port. */
export const freeOperationsEndpoint = async (): Promise<string> =>
  `http://127.0.0.1:${(await freePort()).toString()}`;
