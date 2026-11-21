#!/usr/bin/env node
// The submit-ingress load harness (#871): builds a request corpus, offers it to
// a node's `/submit` at a fixed rate, and writes a report of admitted TPS, the
// refusal mix, latency, Postgres write rates and the node's main-thread CPU.
//
//   node scripts/submit-ingress-load.mjs corpus --out <dir> --valid 300000 \
//     --junk bad-cbor=0.02,bad-signature=0.02,low-fee=0.02,missing-input=0.02,double-spend=0.02
//   node scripts/submit-ingress-load.mjs run --corpus <dir> --url http://127.0.0.1:3000 \
//     --rate 2000 --duration 60 --database-url postgres://... --drain-rate 2000 \
//     --node-pid <pid> --report <file.json>
//   node scripts/submit-ingress-load.mjs drain --database-url postgres://... --rate 2000
//
// The scripts import the built workspace packages: build them first with
// `node scripts/contrib.mjs build --package @al-ft/midgard-core` (and
// `@al-ft/midgard-validation`) from the repository root.
//
// Each run sends every corpus request at most once. A second run against the
// same database sees duplicates for every tx the first admitted; give it a
// fresh corpus seed, a fresh database, or `--offset` past what was sent.
import { parseArgs } from "node:util";

import {
  buildCorpus,
  JUNK_KINDS,
  parseJunkMix,
  readCorpus,
} from "./submit-ingress-load.corpus.mjs";
import { runLoad } from "./submit-ingress-load.client.mjs";
import {
  makeHarnessPg,
  postgresRates,
  samplePostgres,
  startDrainStub,
} from "./submit-ingress-load.drain-stub.mjs";
import {
  cpuShares,
  sampleProcessCpu,
  summaryLines,
  writeReport,
} from "./submit-ingress-load.report.mjs";

const USAGE = `usage:
  submit-ingress-load.mjs corpus --out <dir> --valid <n> [--junk <kind=share,...>]
      [--seed <text>] [--min-fee-a <n>] [--min-fee-b <n>] [--keys <n>]
  submit-ingress-load.mjs run --corpus <dir> --url <base url> --rate <per s> --duration <s>
      [--socket <path>] [--insecure] [--offset <n>] [--max-in-flight <n>]
      [--keep-alive-share <0..1>] [--client-ips <n>] [--no-client-ip-header]
      [--loopback-sources <n>]
      [--timeout-ms <n>] [--database-url <url>] [--drain-rate <per s>]
      [--node-pid <pid>] [--report <file>]
  submit-ingress-load.mjs drain --database-url <url> --rate <per s> [--duration <s>]
junk kinds: ${JUNK_KINDS.join(", ")}
the database URL may also come from MIDGARD_LOAD_DATABASE_URL`;

const fail = (message) => {
  console.error(`${message}\n${USAGE}`);
  process.exit(2);
};

const positiveNumber = (value, name) => {
  const number = Number(value);
  if (!Number.isFinite(number) || number <= 0)
    fail(`--${name} must be a positive number`);
  return number;
};

const corpusCommand = (args) => {
  const { values } = parseArgs({
    args,
    options: {
      out: { type: "string" },
      valid: { type: "string" },
      junk: { type: "string" },
      seed: { type: "string", default: "baseline" },
      "min-fee-a": { type: "string", default: "1" },
      "min-fee-b": { type: "string", default: "2000" },
      keys: { type: "string", default: "1024" },
    },
  });
  if (values.out === undefined) fail("--out is required");
  if (values.valid === undefined) fail("--valid is required");
  const startedAt = performance.now();
  const manifest = buildCorpus({
    directory: values.out,
    seed: values.seed,
    validCount: Math.floor(positiveNumber(values.valid, "valid")),
    junkMix: parseJunkMix(values.junk),
    minFeeA: BigInt(values["min-fee-a"]),
    minFeeB: BigInt(values["min-fee-b"]),
    keyCount: Math.floor(positiveNumber(values.keys, "keys")),
    onProgress: (built, total) =>
      process.stderr.write(`\rbuilt ${built}/${total}`),
  });
  process.stderr.write("\n");
  console.log(
    JSON.stringify({
      directory: values.out,
      seconds: (performance.now() - startedAt) / 1_000,
      ...manifest,
    }),
  );
};

const databaseUrl = (value) => value ?? process.env.MIDGARD_LOAD_DATABASE_URL;

const runCommand = async (args) => {
  const { values } = parseArgs({
    args,
    options: {
      corpus: { type: "string" },
      url: { type: "string" },
      socket: { type: "string" },
      insecure: { type: "boolean", default: false },
      rate: { type: "string" },
      duration: { type: "string" },
      offset: { type: "string", default: "0" },
      "max-in-flight": { type: "string", default: "512" },
      "keep-alive-share": { type: "string", default: "0.1" },
      "client-ips": { type: "string", default: "4096" },
      "no-client-ip-header": { type: "boolean", default: false },
      "loopback-sources": { type: "string", default: "0" },
      "timeout-ms": { type: "string", default: "10000" },
      "database-url": { type: "string" },
      "drain-rate": { type: "string" },
      "node-pid": { type: "string" },
      report: { type: "string" },
    },
  });
  if (values.corpus === undefined) fail("--corpus is required");
  if (values.url === undefined) fail("--url is required");
  const ratePerSecond = positiveNumber(values.rate, "rate");
  const durationMs = positiveNumber(values.duration, "duration") * 1_000;
  const keepAliveShare = Number(values["keep-alive-share"]);
  if (!(keepAliveShare >= 0 && keepAliveShare <= 1))
    fail("--keep-alive-share must be in [0, 1]");
  const dbUrl = databaseUrl(values["database-url"]);
  if (values["drain-rate"] !== undefined && dbUrl === undefined)
    fail("--drain-rate needs --database-url");

  const corpus = readCorpus(values.corpus);
  const offset = Math.floor(Number(values.offset));
  const requests = corpus.requests.slice(offset);
  if (requests.length === 0) fail("--offset is past the end of the corpus");

  const runtime =
    dbUrl === undefined
      ? undefined
      : makeHarnessPg(dbUrl, "submit-ingress-load");
  const nodePid =
    values["node-pid"] === undefined ? undefined : Number(values["node-pid"]);
  try {
    const drain =
      values["drain-rate"] === undefined
        ? undefined
        : startDrainStub({
            runtime,
            ratePerSecond: positiveNumber(values["drain-rate"], "drain-rate"),
          });
    const pgBefore =
      runtime === undefined
        ? undefined
        : await runtime.runPromise(samplePostgres);
    const cpuBefore =
      nodePid === undefined ? undefined : sampleProcessCpu(nodePid);
    const load = await runLoad({
      requests,
      target: {
        url: new URL(values.url),
        socketPath: values.socket,
        insecure: values.insecure,
      },
      ratePerSecond,
      durationMs,
      maxInFlight: Math.floor(
        positiveNumber(values["max-in-flight"], "max-in-flight"),
      ),
      keepAliveShare,
      clientIpCount: Math.floor(
        positiveNumber(values["client-ips"], "client-ips"),
      ),
      useClientIpHeader: !values["no-client-ip-header"],
      loopbackSources: Math.max(
        0,
        Math.floor(Number(values["loopback-sources"])),
      ),
      timeoutMs: positiveNumber(values["timeout-ms"], "timeout-ms"),
      onProgress: ({ elapsedMs, sent, inFlight }) =>
        process.stderr.write(
          `\r${(elapsedMs / 1_000).toFixed(0)} s: sent ${sent}, in flight ${inFlight}   `,
        ),
    });
    process.stderr.write("\n");
    const cpuAfter =
      nodePid === undefined ? undefined : sampleProcessCpu(nodePid);
    const pgAfter =
      runtime === undefined
        ? undefined
        : await runtime.runPromise(samplePostgres);
    const drainStats = drain === undefined ? undefined : await drain.stop();
    const report = {
      corpus: {
        directory: values.corpus,
        seed: corpus.manifest.seed,
        counts: corpus.manifest.counts,
        offset,
      },
      target: {
        url: values.url,
        socket: values.socket ?? null,
        clientIpHeader: !values["no-client-ip-header"],
        loopbackSources: Number(values["loopback-sources"]),
        keepAliveShare,
      },
      load,
      ...(pgBefore === undefined
        ? {}
        : { postgres: postgresRates(pgBefore, pgAfter) }),
      ...(cpuBefore === undefined
        ? {}
        : { nodeCpu: { pid: nodePid, ...cpuShares(cpuBefore, cpuAfter) } }),
      ...(drainStats === undefined ? {} : { drainStub: drainStats }),
    };
    for (const line of summaryLines(report)) console.log(line);
    if (values.report !== undefined)
      console.log(`report: ${writeReport(values.report, report)}`);
    else console.log(JSON.stringify(report));
  } finally {
    await runtime?.dispose();
  }
};

const drainCommand = async (args) => {
  const { values } = parseArgs({
    args,
    options: {
      "database-url": { type: "string" },
      rate: { type: "string" },
      duration: { type: "string" },
    },
  });
  const dbUrl = databaseUrl(values["database-url"]);
  if (dbUrl === undefined) fail("--database-url is required");
  const runtime = makeHarnessPg(dbUrl, "submit-ingress-drain-stub");
  const drain = startDrainStub({
    runtime,
    ratePerSecond: positiveNumber(values.rate, "rate"),
  });
  const stop = async () => {
    const stats = await drain.stop();
    await runtime.dispose();
    console.log(JSON.stringify(stats));
  };
  if (values.duration === undefined) {
    const onSignal = () => void stop().then(() => process.exit(0));
    process.once("SIGINT", onSignal);
    process.once("SIGTERM", onSignal);
    return;
  }
  await new Promise((resolve) =>
    setTimeout(resolve, positiveNumber(values.duration, "duration") * 1_000),
  );
  await stop();
};

const [command, ...rest] = process.argv.slice(2);
const commands = {
  corpus: corpusCommand,
  run: runCommand,
  drain: drainCommand,
};
if (commands[command] === undefined) fail(`unknown command ${String(command)}`);
await commands[command](rest);
