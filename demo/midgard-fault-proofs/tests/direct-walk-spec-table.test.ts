/**
 * The kit's spec table: one literal row per direct-walk family pins what its
 * walk declares (category, step count, role order, opened fields, step-01
 * proof chunk, terminal verdict, journaled artifact version and catalogue
 * category id), and every row is checked against the family definition the
 * kit derives from the walk. The table covers exactly the walks the
 * direct-walk application records are built from.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  DIRECT_PURPOSE_FIELDS,
  type DirectPurposeRole,
} from "../src/direct-purpose-family/index.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK } from "../src/distinct-asset-accumulation-limit/index.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK } from "../src/execution-native-script-invalid/index.js";
import { EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK } from "../src/execution-source-script-decoding/index.js";
import { MISSING_REDEEMER_DIRECT_WALK } from "../src/missing-redeemer/index.js";
import { MISSING_SCRIPT_SOURCE_DIRECT_WALK } from "../src/missing-script-source/index.js";
import { RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK } from "../src/receive-purpose-language/index.js";
import { SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK } from "../src/script-integrity-hash-mismatch/index.js";
import { UNUSED_REDEEMER_DIRECT_WALK } from "../src/unused-redeemer/index.js";
import { UNUSED_SCRIPT_WITNESS_DIRECT_WALK } from "../src/unused-script-witness/index.js";
import { CURSOR_FAMILY_SPECS } from "../src/workflow/cursor-family-spec.js";
import * as DIRECT_WALK_RECORDS from "../src/workflow/family-application-registry.direct-walk-family-application-record.js";
import { CHUNKED_VERIFY_WITNESS_ROLES } from "../src/workflow/family-definition.js";
import type { DirectWalkCategory } from "./support/direct-walk-emulator.js";

/** The members of a walk and its derived definition the table reads. */
type Walk = Readonly<{
  spec: Readonly<{ category: string; stepCount: number }>;
  family: Readonly<{ roleOrder: readonly DirectPurposeRole[] }>;
  terminal?: unknown;
  artifact: Readonly<{ artifactVersion: string }>;
  fields?: readonly number[];
  definition: Readonly<{
    category: string;
    stepDatumSchemas: readonly unknown[];
    witnessRoles: readonly string[];
    fieldPreimageCertificate: boolean;
    adapter: Readonly<{
      kind: string;
      spec?: unknown;
      stepContractNames?: readonly string[];
    }>;
    fieldCarriage?: readonly unknown[];
    proofChunk?: unknown;
  }>;
}>;

type Row = Readonly<{
  walk: Walk;
  category: DirectWalkCategory;
  stepCount: number;
  roleOrder: readonly DirectPurposeRole[];
  fields: readonly number[];
  /** Whether step 01 publishes its inclusion proof as chunks. */
  proofChunk: boolean;
  /** `verdict`: the custody family carries the terminal's verdict;
   * `derived`: the walk builds it from the admitted material and proof. */
  terminal: "verdict" | "derived";
  artifactVersion: string;
  categoryId: string;
}>;

const ROWS: readonly Row[] = [
  {
    walk: MISSING_REDEEMER_DIRECT_WALK,
    category: "missingRedeemer",
    stepCount: 39,
    roleOrder: SDK.MISSING_REDEEMER_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6, 8],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion: "midgard-missing-redeemer-workflow-artifact-v1",
    categoryId: "0000002e",
  },
  {
    walk: MISSING_SCRIPT_SOURCE_DIRECT_WALK,
    category: "missingScriptSource",
    stepCount: 37,
    roleOrder: SDK.MISSING_SCRIPT_SOURCE_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6, 8],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion: "midgard-missing-script-source-workflow-artifact-v1",
    categoryId: "0000002d",
  },
  {
    walk: UNUSED_REDEEMER_DIRECT_WALK,
    category: "unusedRedeemer",
    stepCount: 25,
    roleOrder: SDK.UNUSED_REDEEMER_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6, 8],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion: "midgard-unused-redeemer-workflow-artifact-v1",
    categoryId: "00000030",
  },
  {
    walk: RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK,
    category: "receivePurposeLanguage",
    stepCount: 21,
    roleOrder: SDK.RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6, 8],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion: "midgard-receive-purpose-language-workflow-artifact-v1",
    categoryId: "00000034",
  },
  {
    walk: EXECUTION_SOURCE_SCRIPT_DECODING_DIRECT_WALK,
    category: "executionSourceScriptDecoding",
    stepCount: 3,
    roleOrder: SDK.EXECUTION_SOURCE_SCRIPT_DECODING_ROLE_ORDER,
    fields: [],
    proofChunk: false,
    terminal: "verdict",
    artifactVersion:
      "midgard-execution-source-script-decoding-workflow-artifact-v1",
    categoryId: "00000031",
  },
  {
    walk: DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK,
    category: "distinctAssetAccumulationLimit",
    stepCount: 12,
    roleOrder: SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER,
    fields: [0, 1, 2, 5],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion:
      "midgard-distinct-asset-accumulation-limit-workflow-artifact-v1",
    categoryId: "00000035",
  },
  {
    walk: EXECUTION_NATIVE_SCRIPT_INVALID_DIRECT_WALK,
    category: "executionNativeScriptInvalid",
    stepCount: 28,
    roleOrder: SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6, 7, 8],
    proofChunk: true,
    terminal: "verdict",
    artifactVersion:
      "midgard-execution-native-script-invalid-workflow-artifact-v1",
    categoryId: "00000032",
  },
  {
    walk: SCRIPT_INTEGRITY_HASH_MISMATCH_DIRECT_WALK,
    category: "scriptIntegrityHashMismatch",
    stepCount: 21,
    roleOrder: SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6],
    proofChunk: true,
    terminal: "derived",
    artifactVersion:
      "midgard-script-integrity-hash-mismatch-workflow-artifact-v1",
    categoryId: "00000033",
  },
  {
    walk: UNUSED_SCRIPT_WITNESS_DIRECT_WALK,
    category: "unusedScriptWitness",
    stepCount: 24,
    roleOrder: SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
    fields: [0, 1, 2, 3, 5, 6],
    proofChunk: true,
    terminal: "derived",
    artifactVersion: "midgard-unused-script-witness-workflow-artifact-v1",
    categoryId: "0000002f",
  },
];

describe("the direct-walk spec table", () => {
  it("covers exactly the walks of the direct-walk application records", () => {
    const records = Object.values(DIRECT_WALK_RECORDS);
    expect(records).toHaveLength(ROWS.length);
    for (const record of records) {
      const rows = ROWS.filter(
        ({ walk }) => walk.definition === record.definition,
      );
      expect(rows.map(({ category }) => category)).toStrictEqual([
        record.category,
      ]);
    }
    expect(new Set(ROWS.map(({ category }) => category)).size).toBe(
      ROWS.length,
    );
  });

  describe.each(ROWS)("$category", (row) => {
    const { walk } = row;
    const { definition } = walk;

    it("is the cursor spec of its category", () => {
      expect(walk.spec).toBe(CURSOR_FAMILY_SPECS[row.category]);
      expect(definition.category).toBe(row.category);
      expect(definition.adapter.kind).toBe("cursor");
      expect(definition.adapter.spec).toBe(walk.spec);
    });

    it("declares one step datum, contract and role per step", () => {
      expect(walk.spec.stepCount).toBe(row.stepCount);
      expect(definition.stepDatumSchemas).toHaveLength(row.stepCount);
      expect(definition.adapter.stepContractNames).toHaveLength(row.stepCount);
      expect(walk.family.roleOrder).toBe(row.roleOrder);
      expect(walk.family.roleOrder).toHaveLength(row.stepCount);
    });

    it("walks its roles from step-01 to the terminal", () => {
      expect(walk.family.roleOrder[0]).toBe("step01");
      expect(walk.family.roleOrder.at(-1)).toBe("terminal");
    });

    it("carries one field ahead of its actions per field it opens", () => {
      expect(walk.fields ?? DIRECT_PURPOSE_FIELDS).toStrictEqual(row.fields);
      expect(definition.fieldCarriage).toHaveLength(row.fields.length);
    });

    it("publishes the step-01 proof chunk exactly when its row says so", () => {
      expect("proofChunk" in definition).toBe(row.proofChunk);
      if (row.proofChunk) expect(definition.proofChunk).toBeTypeOf("function");
    });

    it("certifies field preimages with the chunked-verify witnesses", () => {
      expect(definition.fieldPreimageCertificate).toBe(true);
      expect(definition.witnessRoles).toBe(CHUNKED_VERIFY_WITNESS_ROLES);
    });

    it("judges its terminal by the verdict its row names", () => {
      expect(
        "verdict" in walk.family && walk.terminal === undefined
          ? "verdict"
          : !("verdict" in walk.family) && typeof walk.terminal === "function"
            ? "derived"
            : "both or neither",
      ).toBe(row.terminal);
    });

    it("journals its artifact version under its catalogue category id", () => {
      expect(walk.artifact.artifactVersion).toBe(row.artifactVersion);
      expect(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[row.category]).toBe(
        row.categoryId,
      );
    });
  });
});
