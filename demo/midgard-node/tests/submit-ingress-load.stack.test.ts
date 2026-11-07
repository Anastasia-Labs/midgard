// The load harness end to end against a real node in process: the client
// offers a corpus to the production `/submit` behind the production ingress,
// the production admission writer queues what it admits in Postgres, and the
// drain stub then marks every queued row terminal while the Postgres sampler
// counts the rows. The node's own tx-queue processor never runs here, so the
// stub is the only thing that drains.
import "./utils.js";

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

import { Emulator, generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import { runLoad } from "../scripts/submit-ingress-load.client.mjs";
import {
  buildCorpus,
  parseJunkMix,
  readCorpus,
} from "../scripts/submit-ingress-load.corpus.mjs";
import {
  DRAIN_STUB_REJECT_CODE,
  makeHarnessPg,
  postgresRates,
  samplePostgres,
  startDrainStub,
} from "../scripts/submit-ingress-load.drain-stub.mjs";
import { DEFAULT_HTTP_RATE_LIMITS } from "../src/services/http-rate-limits.js";
import { operatorFromSeed, startShardNode } from "./helpers/l2-peer-node.js";

const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "submit-ingress-stack-"));
afterAll(() => fs.rmSync(scratch, { recursive: true, force: true }));

const databaseUrl = () =>
  `postgres://${process.env.POSTGRES_USER ?? "postgres"}:${process.env.POSTGRES_PASSWORD ?? "postgres"}@${process.env.POSTGRES_HOST ?? "127.0.0.1"}:${process.env.POSTGRES_PORT ?? "5433"}/${process.env.POSTGRES_DB ?? ""}`;

describe("submit-ingress load harness against an in-process node", () => {
  it("admits the corpus through /submit, and the drain stub empties the queue", async () => {
    const directory = path.join(scratch, "corpus");
    buildCorpus({
      directory,
      seed: "stack-test",
      validCount: 60,
      junkMix: parseJunkMix(
        "bad-cbor=0.1,bad-signature=0.05,low-fee=0.05,missing-input=0.05,double-spend=0.05",
      ),
      keyCount: 16,
    });
    const corpus = readCorpus(directory);
    const operator = operatorFromSeed(generateSeedPhrase());
    const node = await startShardNode({
      operator,
      activeOperators: [operator.keyHash],
      lucid: await Lucid(new Emulator([]), "Custom"),
      // The harness measures ingress, so the per-client and global write
      // limits (4 admissions/s by default) stay out of the way here. The
      // client stands behind a trusted proxy, as behind the nginx edge: each
      // request counts against its synthetic client address, so one socket
      // peer's four-request concurrency bound does not apply.
      config: {
        MIN_FEE_A: 1n,
        MIN_FEE_B: 2_000n,
        HTTP_TRUSTED_PROXY_PEERS: ["127.0.0.1"],
      },
      ingressPolicy: { ...DEFAULT_HTTP_RATE_LIMITS, enabled: false },
    });
    const pg = makeHarnessPg(databaseUrl(), "submit-ingress-load-test");
    try {
      const before = await pg.runPromise(samplePostgres);
      const load = await runLoad({
        requests: corpus.requests,
        target: { url: new URL(node.url) },
        ratePerSecond: 200,
        durationMs: 30_000,
      });
      // The ingress check refuses only the bad CBOR; every other kind is a
      // well-formed tx that validation, not ingress, refuses.
      const counts = corpus.manifest.counts;
      const queued = corpus.requests.length - counts["bad-cbor"];
      expect({
        sent: load.sent,
        completed: load.completed,
        admitted: load.admitted,
        refusedBadCbor: load.byKind["bad-cbor"],
      }).toEqual({
        sent: corpus.requests.length,
        completed: corpus.requests.length,
        admitted: queued,
        refusedBadCbor: {
          "400 Invalid canonical transaction CBOR payload": counts["bad-cbor"],
        },
      });
      // A 202 is answered from memory and the row follows by group commit,
      // so wait for the writer before counting rows.
      await node.admissions();
      const admitted = await pg.runPromise(samplePostgres);
      expect(admitted.admissions).toEqual({ queued });

      const drain = startDrainStub({
        runtime: pg,
        ratePerSecond: 400,
        tickMs: 25,
      });
      const deadline = performance.now() + 20_000;
      while (
        (await pg.runPromise(samplePostgres)).admissions.queued !== undefined
      ) {
        if (performance.now() > deadline)
          throw new Error("the drain stub left rows queued");
        await new Promise((resolve) => setTimeout(resolve, 50));
      }
      const stats = await drain.stop();
      const after = await pg.runPromise(samplePostgres);
      const rates = postgresRates(before, after);
      expect({
        marked: stats.marked,
        errors: stats.errors,
        admissionsAtEnd: rates.admissionsAtEnd,
        drainedByStub: rates.drainedByStub,
      }).toEqual({
        marked: queued,
        errors: 0,
        admissionsAtEnd: { rejected: queued },
        drainedByStub: queued,
      });
      expect(rates.admissionRowsPerSecond).toBeGreaterThan(0);
      const rows = await node.admissions();
      expect(new Set(rows.map((row) => row.reject_code))).toEqual(
        new Set([DRAIN_STUB_REJECT_CODE]),
      );
    } finally {
      await pg.dispose();
      await node.close();
    }
  });
});
