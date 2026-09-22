import { createHash } from "node:crypto";
import { createServer } from "node:https";
import { appendFileSync, existsSync, readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { setTimeout as pause } from "node:timers/promises";

const pointKeys = ["blockHash", "blockNo", "pointId", "slot"];
const exact = (value, keys) =>
  value !== null &&
  typeof value === "object" &&
  !Array.isArray(value) &&
  Object.keys(value).sort().join() === [...keys].sort().join();
const digest = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/.test(value);
const natural = (value) =>
  typeof value === "string" && /^(0|[1-9][0-9]*)$/.test(value);
const point = (value) =>
  exact(value, pointKeys) &&
  digest(value.blockHash) &&
  digest(value.pointId) &&
  natural(value.blockNo) &&
  natural(value.slot) &&
  value.pointId ===
    createHash("sha256")
      .update(`${value.slot}:${value.blockHash}:${value.blockNo}`)
      .digest("hex");
const samePoint = (left, right) =>
  point(left) &&
  point(right) &&
  pointKeys.every((key) => left[key] === right[key]);
const json = (path) => JSON.parse(readFileSync(path, "utf8"));

/** Dispatch only authenticated retained records; the HTTPS identity is fixed at startup. */
export function createHistoryArchiveDispatch(directory, authority) {
  const canonical = (inclusion, through) => {
    const marker = join(directory, "canonical-ready");
    if (!existsSync(marker)) return false;
    const generation = readFileSync(marker, "utf8");
    if (
      !point(inclusion) ||
      !point(through) ||
      BigInt(inclusion.blockNo) > BigInt(through.blockNo) ||
      BigInt(inclusion.slot) > BigInt(through.slot)
    )
      return false;
    let previousSlot;
    let previousHash;
    for (
      let number = BigInt(inclusion.blockNo);
      number <= BigInt(through.blockNo);
      number++
    ) {
      const path = join(directory, "canonical", `${number}.json`);
      if (!existsSync(path)) return false;
      const entry = json(path);
      const retained = entry.point;
      if (
        !point(retained) ||
        BigInt(retained.blockNo) !== number ||
        (previousSlot !== undefined && BigInt(retained.slot) <= previousSlot)
      )
        return false;
      if (previousHash !== undefined && entry.prevHash !== previousHash)
        return false;
      if (
        number === BigInt(inclusion.blockNo) &&
        !samePoint(retained, inclusion)
      )
        return false;
      if (number === BigInt(through.blockNo) && !samePoint(retained, through))
        return false;
      previousSlot = BigInt(retained.slot);
      previousHash = retained.blockHash;
    }
    return existsSync(marker) && readFileSync(marker, "utf8") === generation;
  };
  return (method, url, body) => {
    const match =
      /^\/midgard\/v1\/historical-payload\/([0-9a-f]{64})\/([0-9a-f]{56})$/.exec(
        url,
      );
    if (
      method === "GET" &&
      match &&
      match[1] === authority.releaseFinality.deploymentIdentityDigest
    ) {
      const path = join(directory, "records", `${match[2]}.json`);
      return existsSync(path)
        ? { status: 200, value: json(path) }
        : { status: 404 };
    }
    if (method !== "POST") return { status: 404 };
    if (url === "/midgard/v1/native-script-publication/canonicality") {
      if (
        !exact(body, ["inclusionPoint", "throughPoint"]) ||
        !point(body.inclusionPoint) ||
        !point(body.throughPoint)
      )
        return { status: 400 };
      return {
        status: 200,
        value: {
          canonical: canonical(body.inclusionPoint, body.throughPoint),
          inclusionPoint: body.inclusionPoint,
          throughPoint: body.throughPoint,
        },
      };
    }
    if (url !== "/midgard/v1/native-script-publication") return { status: 404 };
    if (
      !exact(body, [
        "deploymentIdentityDigest",
        "blueprintHash",
        "finalityPolicyDigest",
        "expectedScriptHash",
        "throughPoint",
      ]) ||
      typeof body.expectedScriptHash !== "string" ||
      !/^[0-9a-f]{56}$/.test(body.expectedScriptHash) ||
      !point(body.throughPoint)
    )
      return { status: 400 };
    const release = authority.releaseFinality;
    if (
      body.deploymentIdentityDigest !== release.deploymentIdentityDigest ||
      body.blueprintHash !== release.blueprintHash ||
      body.finalityPolicyDigest !== release.policyDigest
    )
      return { status: 409 };
    const path = join(directory, "native-scripts", body.expectedScriptHash);
    const candidates = existsSync(path)
      ? readdirSync(path)
          .filter((file) => file.endsWith(".json"))
          .map((file) => json(join(path, file)))
      : [];
    const eligible = candidates.filter(
      (record) =>
        record.expectedScriptHash === body.expectedScriptHash &&
        canonical(record.inclusionPoint, body.throughPoint) &&
        BigInt(body.throughPoint.blockNo) -
          BigInt(record.inclusionPoint.blockNo) +
          1n >=
          BigInt(release.policy.confirmationDepth),
    );
    eligible.sort(
      (a, b) =>
        Number(
          BigInt(a.inclusionPoint.blockNo) - BigInt(b.inclusionPoint.blockNo),
        ) || a.publicationOutRef.localeCompare(b.publicationOutRef),
    );
    const record = eligible[0];
    if (record === undefined) return { status: 404 };
    return {
      status: 200,
      value: {
        schemaVersion: "midgard-historical-native-script-evidence-v1",
        deploymentIdentityDigest: release.deploymentIdentityDigest,
        blueprintHash: release.blueprintHash,
        finalityPolicyDigest: release.policyDigest,
        expectedScriptHash: record.expectedScriptHash,
        sourceMode: "external_providers",
        sourceId: authority.sourceId,
        operatorIdentitySha256: authority.operatorIdentitySha256,
        scriptBytesHex: record.scriptBytesHex,
        publicationOutRef: record.publicationOutRef,
        publicationOutputCbor: record.publicationOutputCbor,
        publicationTransactionBodyCbor: record.publicationTransactionBodyCbor,
        publicationTransactionIndex: record.publicationTransactionIndex,
        inclusionBlockTransactionIds: record.inclusionBlockTransactionIds,
        inclusionPoint: record.inclusionPoint,
        throughPoint: body.throughPoint,
      },
    };
  };
}

export async function startHistoryArchiveServer(directory = "/service") {
  while (!existsSync(join(directory, "ready"))) await pause(100);
  const dispatch = createHistoryArchiveDispatch(
    directory,
    json(join(directory, "authority.json")),
  );
  const server = createServer(
    {
      key: readFileSync(join(directory, "key.pem")),
      cert: readFileSync(join(directory, "certificate.pem")),
    },
    async (request, response) => {
      try {
        const chunks = [];
        let size = 0;
        for await (const chunk of request) {
          size += chunk.length;
          if (size > 16_384) {
            response.writeHead(413).end();
            return;
          }
          chunks.push(chunk);
        }
        const body =
          size === 0
            ? undefined
            : JSON.parse(Buffer.concat(chunks).toString("utf8"));
        const result = dispatch(request.method, request.url, body);
        appendFileSync(
          join(directory, "requests.ndjson"),
          JSON.stringify({
            at: new Date().toISOString(),
            method: request.method,
            url: request.url,
            status: result.status,
          }) + "\n",
        );
        response
          .writeHead(result.status, { "content-type": "application/json" })
          .end(result.value === undefined ? "" : JSON.stringify(result.value));
      } catch {
        response.writeHead(400).end();
      }
    },
  );
  server.listen(8443, "0.0.0.0");
  process.on("SIGTERM", () => server.close(() => process.exit(0)));
  return server;
}

if (process.argv[1] === fileURLToPath(import.meta.url))
  await startHistoryArchiveServer();
