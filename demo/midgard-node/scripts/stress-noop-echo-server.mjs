#!/usr/bin/env node

import { createHash } from "node:crypto";
import http from "node:http";

const port = Number.parseInt(process.env.STRESS_NOOP_PORT ?? "3099", 10);
const host = process.env.STRESS_NOOP_HOST ?? "127.0.0.1";
const maxBytes = Number.parseInt(
  process.env.STRESS_NOOP_MAX_BYTES ?? "1048576",
  10,
);

const isApplicationCbor = (value) =>
  typeof value === "string" &&
  value
    .split(";")[0]
    .trim()
    .toLowerCase() === "application/cbor";

const readBody = async (request) => {
  const chunks = [];
  let total = 0;
  for await (const chunk of request) {
    total += chunk.length;
    if (total > maxBytes) {
      throw new Error("request body exceeded STRESS_NOOP_MAX_BYTES");
    }
    chunks.push(chunk);
  }
  return Buffer.concat(chunks);
};

const server = http.createServer(async (request, response) => {
  if (request.method !== "POST" || request.url !== "/submit") {
    response.writeHead(404, { "content-type": "application/json" });
    response.end(JSON.stringify({ error: "not_found" }));
    return;
  }
  if (!isApplicationCbor(request.headers["content-type"])) {
    response.writeHead(415, { "content-type": "application/json" });
    response.end(JSON.stringify({ error: "expected_application_cbor" }));
    return;
  }
  try {
    const body = await readBody(request);
    const txId = createHash("sha256").update(body).digest("hex");
    response.writeHead(202, { "content-type": "application/json" });
    response.end(JSON.stringify({ txId }));
  } catch (error) {
    response.writeHead(413, { "content-type": "application/json" });
    response.end(
      JSON.stringify({
        error: error instanceof Error ? error.message : String(error),
      }),
    );
  }
});

server.listen(port, host, () => {
  console.log(`stress-noop-echo-server listening on http://${host}:${port}`);
});
