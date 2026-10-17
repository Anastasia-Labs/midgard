/**
 * Types for `ledger-output-proof-replay.vectors.mjs`.
 *
 * The rows are authored as plain ESM so the generator can load them on bare
 * `node` without a build step; this file lets the vitest suites consume them
 * with the types the trace builder exposes. A name here that no longer exists
 * in `src/` degrades silently to `any`, so the export list is checked against
 * the real module by `tests/ledger-output-proof-split-span.test.ts`, and every
 * row is driven through the real trace builder and compared against the
 * checked-in fixture.
 */

import type {
  buildMidgardLedgerOutputProofTrace,
  encodeMidgardLedgerOutputProofControl,
  isExactMidgardLedgerOutputProofTerminal,
  MidgardLedgerOutputProofStages,
  MidgardLedgerOutputProofWitness,
  nextMidgardCekDataTraverseSpan,
} from "@al-ft/midgard-core";
import type {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import type { Constr, Data } from "@lucid-evolution/lucid";

import type { plutusDataToCborHex } from "../../src/plutus-data.js";

/** One row: an output and the claims its proof must exhibit. */
export type LedgerOutputProofReplayRowV1 = {
  readonly id: string;
  readonly label: string;
  readonly datumHex: string;
  readonly scriptHex?: string;
  readonly asset?: {
    readonly policyHex: string;
    readonly nameHex: string;
    readonly quantity: bigint;
  };
  readonly total?: number;
  readonly steps: number;
  readonly emptySteps: number;
  readonly attaches?: readonly number[];
};

export type LedgerOutputProofReplayFamilyV1 = "emptyField" | "datumScalar";

/** What the twins produce for one row, as the generated fixture holds it. */
export type LedgerOutputProofReplayVectorV1 = {
  readonly id: string;
  readonly outputCborHex: string;
  readonly outputLength: number;
  readonly steps: number;
  readonly emptySteps: number;
  readonly attaches: readonly number[];
  readonly exactTerminal: boolean;
  readonly witnessesCborHex: string;
  readonly witnessesHash: string;
  readonly terminalHash: string;
  readonly plannerClaimsCborHex: string;
};

/** The package functions a vector is computed with. */
export type LedgerOutputProofReplayTwinsV1 = {
  readonly buildMidgardLedgerOutputProofTrace: typeof buildMidgardLedgerOutputProofTrace;
  readonly decodeMidgardDatum: typeof decodeMidgardDatum;
  /** `deriveLedgerOutputProofStepClaims` of `@al-ft/midgard-fault-proofs`. */
  readonly deriveLedgerOutputProofStepClaims: (step: {
    readonly control: readonly Data[];
    readonly roleIndex: number;
  }) => { readonly claimedScalar: Data };
  readonly encodeMidgardLedgerOutputProofControl: typeof encodeMidgardLedgerOutputProofControl;
  readonly encodeMidgardTxOutput: typeof encodeMidgardTxOutput;
  readonly isExactMidgardLedgerOutputProofTerminal: typeof isExactMidgardLedgerOutputProofTerminal;
  /** `ledgerOutputProofDatumRoleIndex` of `@al-ft/midgard-fault-proofs`. */
  readonly ledgerOutputProofDatumRoleIndex: (
    control: readonly Data[],
    action: Constr<Data>,
  ) => number;
  readonly MidgardLedgerOutputProofStages: typeof MidgardLedgerOutputProofStages;
  readonly nextMidgardCekDataTraverseSpan: typeof nextMidgardCekDataTraverseSpan;
  readonly plutusDataToCborHex: typeof plutusDataToCborHex;
  readonly witnessData: (
    witness: MidgardLedgerOutputProofWitness,
  ) => Constr<unknown>;
};

export declare const EMPTY_FIELD_ROWS: readonly LedgerOutputProofReplayRowV1[];

export declare const DATUM_SCALAR_ROWS: readonly LedgerOutputProofReplayRowV1[];

export declare const LEDGER_OUTPUT_PROOF_REPLAY_ROWS: readonly {
  readonly row: LedgerOutputProofReplayRowV1;
  readonly family: LedgerOutputProofReplayFamilyV1;
}[];

export declare const ledgerOutputProofReplayVector: (
  row: LedgerOutputProofReplayRowV1,
  family: LedgerOutputProofReplayFamilyV1,
  twins: LedgerOutputProofReplayTwinsV1,
) => LedgerOutputProofReplayVectorV1;

export declare const ledgerOutputProofReplayClaimProblems: (
  row: LedgerOutputProofReplayRowV1,
  vector: LedgerOutputProofReplayVectorV1,
) => string[];
