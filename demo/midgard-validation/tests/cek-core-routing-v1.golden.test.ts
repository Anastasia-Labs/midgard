import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { cekCoreFailureRoute, cekCoreGroup } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { midgardCekSemanticGroup } from "../src/cek-semantic-group.js";
import {
  type CekCoreRoutingRows,
  routingRows,
} from "./fixtures/cek-core-routing-v1.vectors.mjs";

/**
 * The TypeScript half of the CEK core routing golden channel.
 *
 * The rows are rebuilt here from the twins' source, which the generator
 * drives through `dist/`, and compared with the checked-in fixture. The
 * generated Aiken module
 * (`onchain/aiken/validators/fraud-proofs/validation-trace/cek-core-routing-golden.test.ak`)
 * holds `cek_semantic_chain.core_group`, `semantic_group` and group 7's roots
 * (`cek_core_semantic_failure_roots`) to the same rows.
 * Regenerate both with `pnpm run fixtures:cek-core-routing-v1:sync`.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL("./fixtures/cek-core-routing-v1.generated.json", import.meta.url),
    ),
    "utf8",
  ),
) as CekCoreRoutingRows & { readonly generator: string };

const rows = routingRows({
  coreGroup: cekCoreGroup,
  semanticGroup: midgardCekSemanticGroup,
  failureRoute: cekCoreFailureRoute,
});

describe("CEK core routing golden vectors", () => {
  it("routes and refuses each row as the fixture records", () => {
    expect(rows.coreGroup).toEqual(golden.coreGroup);
    expect(rows.semanticGroup).toEqual(golden.semanticGroup);
    expect(rows.failureRoute).toEqual(golden.failureRoute);
  });

  it("routes each core group, each semantics group and each failure route somewhere", () => {
    expect(
      [...new Set(rows.coreGroup.routed.map(({ group }) => group))].sort(
        (left, right) => left - right,
      ),
    ).toEqual(Array.from({ length: 15 }, (_, group) => group));
    expect(
      [...new Set(rows.semanticGroup.routed.map(({ group }) => group))].sort(
        (left, right) => left - right,
      ),
    ).toEqual(Array.from({ length: 13 }, (_, group) => group));
    expect(
      [...new Set(rows.failureRoute.routed.map(({ route }) => route))].sort(),
    ).toEqual(["loop", "material", "staged"]);
  });
});
