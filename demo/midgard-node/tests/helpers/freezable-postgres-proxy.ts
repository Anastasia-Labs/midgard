/**
 * A TCP proxy in front of a test database whose open connections can be
 * frozen: a frozen connection forwards nothing either way and never closes,
 * as a half-open socket does. Connections opened after the freeze forward
 * as usual.
 */
import { once } from "node:events";
import { type AddressInfo, connect, createServer, type Socket } from "node:net";

export const freezablePostgresProxy = async (connectionString: string) => {
  const target = new URL(connectionString);
  const frozen = new WeakSet<Socket>();
  const open = new Set<Socket>();
  let accepted = 0;
  const server = createServer((downstream) => {
    accepted += 1;
    const upstream = connect(Number(target.port), target.hostname);
    open.add(downstream);
    open.add(upstream);
    const forward = (from: Socket, to: Socket): void => {
      from.on("data", (chunk: Buffer) => {
        if (!frozen.has(from)) to.write(chunk);
      });
      from.on("close", () => {
        open.delete(from);
        if (!frozen.has(from)) to.destroy();
      });
      from.on("error", () => undefined);
    };
    forward(downstream, upstream);
    forward(upstream, downstream);
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  const url = new URL(connectionString);
  url.hostname = "127.0.0.1";
  url.port = String((server.address() as AddressInfo).port);
  return {
    url: url.toString(),
    /** Freezes every connection open now. */
    freeze: () => {
      for (const socket of open) frozen.add(socket);
    },
    /** Connections accepted so far. */
    accepted: () => accepted,
    close: async () => {
      for (const socket of open) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};
