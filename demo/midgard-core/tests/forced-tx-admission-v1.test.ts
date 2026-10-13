import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  MIDGARD_FORCED_TX_ADMISSION_CHECKS,
  type MidgardForcedTxAdmissionInput,
  screenMidgardForcedTxAdmission,
} from "../src/consensus-validation.screen-midgard-forced-tx-admission.js";

/**
 * The TypeScript half of the `forced-tx-admission-v1` channel
 * (`scripts/generate-forced-tx-admission-v1-goldens.mjs`): every row's
 * hand-written label, against the screen the SDK and the node run. The Aiken
 * half runs the tx-order mint's rules on the same rows.
 */

type Row = {
  readonly name: string;
  readonly expected: string | null;
  readonly fields: readonly string[];
  readonly validityIntervalEnd: string;
  readonly scriptIntegrityHash: string;
  readonly auxiliaryDataHash: string;
  readonly networkId: string;
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/forced-tx-admission-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly checks: readonly string[]; readonly rows: readonly Row[] };

const input = (row: Row): MidgardForcedTxAdmissionInput => {
  const field = (index: number) => Buffer.from(row.fields[index]!, "hex");
  return {
    body: {
      spendInputsPreimageCbor: field(0),
      referenceInputsPreimageCbor: field(1),
      outputsPreimageCbor: field(2),
      validityIntervalEnd: BigInt(row.validityIntervalEnd),
      requiredObserversPreimageCbor: field(3),
      requiredSignersPreimageCbor: field(4),
      mintPreimageCbor: field(5),
      scriptIntegrityHash: Buffer.from(row.scriptIntegrityHash, "hex"),
      auxiliaryDataHash: Buffer.from(row.auxiliaryDataHash, "hex"),
      networkId: BigInt(row.networkId),
    },
    witnessSet: {
      addrTxWitsPreimageCbor: field(7),
      scriptTxWitsPreimageCbor: field(6),
      redeemerTxWitsPreimageCbor: field(8),
    },
  };
};

describe("forced-tx-admission-v1 golden rows", () => {
  it("names the checks the Aiken half was generated with, in its order", () => {
    expect(golden.checks).toEqual(MIDGARD_FORCED_TX_ADMISSION_CHECKS);
  });

  it("refuses and admits rows from every rule", () => {
    const refused = new Set(
      golden.rows.flatMap(({ expected }) =>
        expected === null ? [] : [expected],
      ),
    );
    expect([...refused].sort()).toEqual(
      [...MIDGARD_FORCED_TX_ADMISSION_CHECKS].sort(),
    );
    expect(golden.rows.some(({ expected }) => expected === null)).toBe(true);
  });

  it.each(golden.rows.map((row) => [row.name, row] as const))(
    "%s",
    (_name, row) => {
      expect(screenMidgardForcedTxAdmission(input(row))?.check ?? null).toBe(
        row.expected,
      );
    },
  );
});
