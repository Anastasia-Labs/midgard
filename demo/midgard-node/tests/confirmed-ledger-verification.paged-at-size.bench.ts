/**
 * A full verification run on the node database (`verifyConfirmedLedger`,
 * its roots derived in its worker thread) over a 300,000-entry
 * `confirmed_ledger`, read a page at a time, with a processed row after
 * the frontier: it passes, under the worker's fixed heap limit. It takes
 * about 300 s, so it is not in the default suite
 * (`confirmed-ledger-verification.paged.test.ts` runs the same over a few
 * pages there); it needs Postgres and `dist`:
 *
 *   pnpm --dir demo --filter midgard-node run build
 *   pnpm --dir demo --filter midgard-node run bench:confirmed-ledger:verification
 */
import { beforeAll, describe, expect, it } from "vitest";

import { verifyConfirmedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { run } from "./commit-base-resolver.fixture.js";
import { seedPaged } from "./confirmed-ledger-verification.paged.fixture.js";
import { provisionMidgardNodeTestDatabaseShards } from "./global-setup.js";

// The bench config has no global setup: create and migrate this worker's
// shard as the default suite's global setup does.
beforeAll(
  () =>
    provisionMidgardNodeTestDatabaseShards(
      Number(process.env.VITEST_POOL_ID ?? "1"),
    ),
  120_000,
);

describe(
  "a full verification run, paged, at size",
  { concurrent: false },
  () => {
    it("passes at 300,000 entries under the worker's fixed heap limit", async () => {
      const count = 300_000;
      const { r0, r1 } = await seedPaged(count);
      const outcome = await run(verifyConfirmedLedger());
      expect(outcome).toMatchObject({
        kind: "verified",
        frontier: { utxosRoot: r0 },
        tail: { utxosRoot: r1 },
        entries: count,
      });
    }, 1_800_000);
  },
);
