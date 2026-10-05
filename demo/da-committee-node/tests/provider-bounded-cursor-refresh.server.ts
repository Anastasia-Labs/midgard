import { createHash } from "node:crypto";
import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import type { Duplex } from "node:stream";

export const chainPoint = (slot: number, branch = 0) => ({
  slot,
  id: createHash("sha256").update(`${branch}:${slot}`).digest("hex"),
});
type Point = ReturnType<typeof chainPoint>;

/** Minimal real TCP WebSocket fixture for the JSON-RPC messages this test owns. */
export const cursorServer = async () => {
  let chain: Point[] = [chainPoint(10)];
  let heldMethod: string | undefined;
  let onHeld: (() => void) | undefined;
  let onClosed: (() => void) | undefined;
  let received = 0;
  const sockets = new Set<Duplex>();
  const server = createServer();
  server.on("upgrade", (request, socket) => {
    const key = request.headers["sec-websocket-key"];
    if (typeof key !== "string") {
      socket.destroy();
      return;
    }
    const accept = createHash("sha1")
      .update(`${key}258EAFA5-E914-47DA-95CA-C5AB0DC85B11`)
      .digest("base64");
    socket.write(
      `HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`,
    );
    sockets.add(socket);
    socket.on("close", () => {
      sockets.delete(socket);
      onClosed?.();
    });
    socket.on("error", () => socket.destroy());
    let at = chain[0]!;
    let buffered = Buffer.alloc(0);
    const send = (id: unknown, result: unknown) => {
      const payload = Buffer.from(
        JSON.stringify({ jsonrpc: "2.0", id, result }),
      );
      const header =
        payload.length < 126
          ? Buffer.from([0x81, payload.length])
          : Buffer.alloc(4);
      if (payload.length >= 126) {
        header[0] = 0x81;
        header[1] = 126;
        header.writeUInt16BE(payload.length, 2);
      }
      socket.write(Buffer.concat([header, payload]));
    };
    socket.on("data", (chunk: Buffer) => {
      buffered = Buffer.concat([buffered, chunk]);
      while (buffered.length >= 2) {
        const opcode = buffered[0]! & 15;
        let length = buffered[1]! & 127;
        let offset = 2;
        if (length === 126) {
          if (buffered.length < 4) return;
          length = buffered.readUInt16BE(2);
          offset = 4;
        }
        if (length === 127 || (buffered[1]! & 128) === 0) {
          socket.destroy();
          return;
        }
        if (buffered.length < offset + 4 + length) return;
        const mask = buffered.subarray(offset, offset + 4);
        const payload = Buffer.from(
          buffered.subarray(offset + 4, offset + 4 + length),
        );
        for (let i = 0; i < payload.length; i++)
          payload[i] = payload[i]! ^ mask[i % 4]!;
        buffered = buffered.subarray(offset + 4 + length);
        if (opcode === 8) {
          socket.end(Buffer.from([0x88, 0]));
          return;
        }
        if (opcode !== 1) {
          socket.destroy();
          return;
        }
        const rpc = JSON.parse(payload.toString("utf8")) as {
          id: unknown;
          method: string;
          params: { points?: Point[] };
        };
        received++;
        if (rpc.method === heldMethod) {
          onHeld?.();
          continue;
        }
        const tip = chain.at(-1)!;
        if (rpc.method === "queryNetwork/genesisConfiguration")
          send(rpc.id, { networkMagic: 424242 });
        else if (rpc.method === "queryNetwork/tip") send(rpc.id, tip);
        else if (rpc.method === "findIntersection") {
          at =
            rpc.params.points?.find((p) => chain.some((c) => c.id === p.id)) ??
            chain[0]!;
          send(rpc.id, { intersection: at, tip });
        } else if (rpc.method === "nextBlock") {
          const index = chain.findIndex((p) => p.id === at.id);
          if (index === -1) {
            at = chain[0]!;
            send(rpc.id, { direction: "backward", point: at, tip });
          } else if (index + 1 < chain.length) {
            at = chain[index + 1]!;
            send(rpc.id, { direction: "forward", block: at, tip });
          }
        } else throw new Error(`Unexpected fixture RPC ${rpc.method}`);
      }
    });
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  return {
    url: `ws://127.0.0.1:${(server.address() as AddressInfo).port}`,
    setChain: (points: Point[]) => {
      chain = points;
    },
    hold: (method: string) => {
      heldMethod = method;
      return new Promise<void>((resolve) => {
        onHeld = resolve;
      });
    },
    nextClose: () =>
      new Promise<void>((resolve) => {
        onClosed = resolve;
      }),
    requests: () => received,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};
