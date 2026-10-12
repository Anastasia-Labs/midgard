import { EMPTY_NULL_ROOT } from "@al-ft/midgard-core";
import {
  computeHash32,
  EMPTY_CBOR_LIST,
  encodeMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { expect } from "vitest";

import type { ScriptIntegrityHashMissingContracts } from "../src/script-integrity-hash-missing/contracts.js";
import {
  advanceFieldGrammarCheckpoint,
  decodeFieldGrammarCheckpoint,
  encodeFieldGrammarCheckpoint,
  initialFieldGrammarCheckpoint,
} from "../src/staged-field-walk/index.js";
import { expectRegisteredChainParity } from "./support/emulator/registered-chain.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  makeFaultProofEmulatorHarness,
  network,
} from "./support/submit-init-emulator-shared.js";

export const field8Checkpoint = (
  checkpoint: ReturnType<typeof initialFieldGrammarCheckpoint>,
) => ({ ...checkpoint, fieldIndex: 8 });

export const advanceField8 = (
  checkpoint: ReturnType<typeof field8Checkpoint>,
  items: readonly Uint8Array[],
  budget = 32,
) =>
  field8Checkpoint(
    advanceFieldGrammarCheckpoint({
      checkpoint: { ...checkpoint, fieldIndex: 6 },
      items,
      budget,
    }),
  );

export const encodeField8 = (
  checkpoint: ReturnType<typeof field8Checkpoint>,
): Buffer => {
  const bytes = encodeFieldGrammarCheckpoint({
    ...checkpoint,
    fieldIndex: 6,
  });
  bytes[36] = 8;
  return bytes;
};

export const decodeField8 = (
  bytes: Uint8Array,
): ReturnType<typeof field8Checkpoint> => {
  const canonicalField6Bytes = Buffer.from(bytes);
  canonicalField6Bytes[36] = 6;
  return field8Checkpoint(decodeFieldGrammarCheckpoint(canonicalField6Bytes));
};

export const hashField8 = (
  checkpoint: ReturnType<typeof field8Checkpoint>,
): string =>
  computeHash32(
    Buffer.concat([
      Buffer.from("MidgardFieldGrammarCheckpointV1", "ascii"),
      encodeField8(checkpoint),
    ]),
  ).toString("hex");

export const REASON = "ScriptIntegrityHashMissing";

export const ABSENT_HASH = EMPTY_NULL_ROOT.toString("hex");

/** Every seam a step authenticates before it reads or commits anything. */
export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "forced_leaf",
  "compact_tx",
  "witness_set_anchor",
  "field_preimage",
  "field_certificate",
  "checkpoint",
] as const;

/** The seven physical scripts, in chain order; every one carries a cancel arm. */
export const PHYSICAL_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "script-grammar",
  "script-scan",
  "redeemer-grammar",
  "step-04",
] as const;

export const coverage = createLifecycleCoverageRecorder();

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/**
 * The registered chain is the deployed identity: the harness folds its first
 * step into the catalogue root. A fresh application of the same blueprint and
 * shared policies must reproduce it step for step before a suite drives it.
 */
export const registeredFamily = async (harness: Harness) => {
  const registered =
    harness.contracts.fraudProofContracts.scriptIntegrityHashMissing;
  const category = harness.catalogue.categories.scriptIntegrityHashMissing!;
  const applied = await Effect.runPromise(
    SDK.buildScriptIntegrityHashMissingFaultProofContracts({
      blueprint: SDK.parseFaultProofBlueprint(
        structuredClone(harness.realBlueprint),
      ),
      network,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      fraudProofCataloguePolicyId:
        harness.contracts.fraudProofCatalogue.policyId,
    }),
  );
  expectRegisteredChainParity({
    registered,
    applied: applied.scriptIntegrityHashMissing.steps,
    category,
  });
  expect(applied.computationThread.policyId).toBe(
    harness.contracts.computationThread.policyId,
  );
  expect(applied.fraudProof.policyId).toBe(
    harness.contracts.fraudProof.policyId,
  );
  const family: ScriptIntegrityHashMissingContracts = {
    steps: registered.steps,
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    fieldPreimageCertificateMintingScript:
      harness.contracts.fieldPreimageCertificate.mintingScript,
    hubOraclePolicyId: harness.contracts.hubOracle.policyId,
    stateQueuePolicyId: harness.contracts.stateQueue.policyId,
  };
  return { family, category };
};

export const nativeTxOf = ({
  scriptItems,
  redeemerItems,
  scriptIntegrityHash,
  fee,
}: {
  readonly scriptItems: readonly Buffer[];
  readonly redeemerItems: readonly Buffer[];
  readonly scriptIntegrityHash: Buffer;
  readonly fee: bigint;
}) =>
  materializeMidgardNativeTxFromCanonical({
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash,
      auxiliaryDataHash: Buffer.alloc(32),
      fee,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: encodeCbor([...scriptItems]),
      redeemerTxWitsPreimageCbor: encodeCbor([...redeemerItems]),
    },
  });

export const plutusScript = (byte: number) =>
  encodeMidgardVersionedScript({
    language: "PlutusV3",
    scriptBytes: Buffer.from([byte]),
  });

export const FORCED_ORDER_KEY = {
  transactionId: "ab".repeat(32),
  outputIndex: 0n,
};
