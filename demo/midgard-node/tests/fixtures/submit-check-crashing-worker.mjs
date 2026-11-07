// The production submit check worker (`dist/submit-check.js`) with two ways
// to die: a batch holding the body 0xdead exits the thread with code 3, and
// one holding 0xbad0 throws an error nothing handles. Every other batch is
// checked by the production worker.
import { parentPort } from "node:worker_threads";

const EXIT = Buffer.from("dead", "hex");
const THROW = Buffer.from("bad0", "hex");

const holds = (bodies, magic) =>
  bodies.some((body) => Buffer.from(new Uint8Array(body)).equals(magic));

// The production worker registers its message listener on import; it gets
// each batch through this filter.
const on = parentPort.on.bind(parentPort);
parentPort.on = (event, listener) =>
  on(
    event,
    event !== "message"
      ? listener
      : (message) => {
          if (holds(message.bodies, EXIT)) process.exit(3);
          if (holds(message.bodies, THROW))
            throw new Error("submit check worker test fault");
          listener(message);
        },
  );

await import(new URL("../../dist/submit-check.js", import.meta.url).href);
