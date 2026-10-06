import { createServer, type Server, Socket } from "node:net";

// The same variables global-setup and the node read: CI serves Postgres on
// 5432 through POSTGRES_PORT, a local checkout on 5433.
export const PG_HOST = process.env.POSTGRES_HOST ?? "127.0.0.1";
export const PG_PORT = Number(process.env.POSTGRES_PORT ?? "5433");
export const PG_USER = process.env.POSTGRES_USER ?? "postgres";
export const PG_PASSWORD = process.env.POSTGRES_PASSWORD ?? "postgres";

export const servers: Server[] = [];
// server.close waits for every connection, and nothing ends a stalled one.
export const silentSockets: Socket[] = [];

/** The ErrorResponse a Postgres still replaying its WAL sends at startup. */
const startingUpResponse = () => {
  const fields = Buffer.from(
    "SFATAL\0VFATAL\0C57P03\0Mthe database system is starting up\0\0",
  );
  const header = Buffer.alloc(5);
  header.write("E", 0);
  header.writeInt32BE(fields.length + 4, 1);
  return Buffer.concat([header, fields]);
};

const listen = async (server: Server) => {
  await new Promise<void>((resolve) =>
    server.listen(0, "127.0.0.1", () => resolve()),
  );
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("proxy has no port");
  }
  return address.port;
};

/**
 * How the proxy answers a connection: forward it to the test Postgres,
 * refuse it the way a restarting Postgres does (57P03), accept it and never
 * answer (a stalled host), accept it and close it before answering (a
 * docker userland proxy in front of a dead Postgres), or accept it and reset
 * it (a connection torn down mid-flight).
 */
type ProxyAnswer = "forward" | "starting_up" | "stall" | "close" | "reset";

/** A TCP proxy to the test Postgres answering connection `n` (from 1). */
export const startProxy = async (answer: (n: number) => ProxyAnswer) => {
  let accepted = 0;
  const clients = new Set<Socket>();
  const server = createServer((client) => {
    accepted += 1;
    client.on("error", () => undefined);
    switch (answer(accepted)) {
      case "starting_up":
        client.once("data", () => client.end(startingUpResponse()));
        return;
      case "stall":
        silentSockets.push(client);
        return;
      case "close":
        // Read (and drop) what the client sends, or the socket never sees
        // its end and never closes.
        client.resume();
        client.end();
        return;
      case "reset":
        client.resetAndDestroy();
        return;
      case "forward": {
        clients.add(client);
        const upstream = new Socket();
        upstream.connect(PG_PORT, PG_HOST, () => {
          client.pipe(upstream).pipe(client);
        });
        upstream.on("error", () => client.destroy());
        client.on("close", () => {
          clients.delete(client);
          upstream.destroy();
        });
      }
    }
  });
  servers.push(server);
  const port = await listen(server);
  return {
    port,
    accepted: () => accepted,
    /** How many forwarded connections are still open. */
    forwarding: () => clients.size,
    /** Closes every forwarded connection, as a backend that went away. */
    dropForwarded: () => clients.forEach((client) => client.end()),
  };
};

/** Refuses the first `refusals` connections (57P03), then forwards. */
export const startRestartingPostgres = (refusals: number) =>
  startProxy((n) => (n <= refusals ? "starting_up" : "forward"));

/** Stalls the first `stalls` connections, then forwards. */
export const startStalledPostgres = (stalls: number) =>
  startProxy((n) => (n <= stalls ? "stall" : "forward"));

/** Closes the first `closes` connections before answering, then forwards. */
export const startDroppingPostgres = (closes: number) =>
  startProxy((n) => (n <= closes ? "close" : "forward"));

/** A port nothing listens on: every connect is refused. */
export const closedPort = async () => {
  const server = createServer();
  const port = await listen(server);
  await new Promise((resolve) => server.close(resolve));
  return port;
};
