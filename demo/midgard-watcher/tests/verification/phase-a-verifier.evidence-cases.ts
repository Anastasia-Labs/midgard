import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import {
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  plutusV3ScriptWitness,
  TEST_ADDRESS_BYTES,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import type { QueuedTx } from "@al-ft/midgard-validation/types";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-snapshot.js";
import { type WatcherHeaderRootReconstructionResult } from "../../src/verification/header-root-reconstruction.js";
import {
  assetMap,
  CONFIG,
  configFor,
  type EvidenceCase,
  evidenceCase,
  fromNativeTx,
  KEY,
  manyOutputs,
  queuedTx,
} from "./phase-a-verifier.base-header.js";

/**
 * One deterministic rejection-evidence case per reachable-and-producible
 * canonical code. The `code`/`stage` columns are asserted against
 * `validatePhaseASingle` itself, so they pin which canonical outcome the
 * fixture reaches; they are never a second opinion about what it should be.
 */
export const EVIDENCE_CASES: readonly EvidenceCase[] = [
  evidenceCase(
    "undecodable transaction bytes",
    RejectCodes.CborDeserialization,
    "canonicalDecode",
    queuedTx(Buffer.alloc(32), Buffer.from([0xff])),
  ),
  evidenceCase(
    "queued tx id does not match the native tx id",
    RejectCodes.TxHashMismatch,
    "compactBinding",
    queuedTx(Buffer.alloc(32, 9), makeNativeTx({ privateKey: KEY }).txCbor),
  ),
  evidenceCase(
    "no spend inputs",
    RejectCodes.EmptyInputs,
    "inputSets",
    fromNativeTx({ spendInputs: [] }),
  ),
  evidenceCase(
    "the same out-ref spent twice",
    RejectCodes.DuplicateInputInTx,
    "inputSets",
    fromNativeTx({ spendInputs: [outRefFromByte(1), outRefFromByte(1)] }),
  ),
  evidenceCase(
    "output preimage item is not a canonical output",
    RejectCodes.InvalidOutput,
    "canonicalDecode",
    fromNativeTx({ outputs: [Buffer.from([0x00])] }),
  ),
  evidenceCase(
    "duplicate required observer",
    RejectCodes.InvalidFieldType,
    "phaseAScriptPreconditions",
    fromNativeTx({
      requiredObserverItems: [Buffer.alloc(28, 3), Buffer.alloc(28, 3)],
      networkId: 0n,
    }),
  ),
  evidenceCase(
    "validity interval start after end",
    RejectCodes.InvalidValidityIntervalFormat,
    "inputSets",
    fromNativeTx({ validityIntervalStart: 5n, validityIntervalEnd: 4n }),
  ),
  evidenceCase(
    "fee below the header-committed minimum",
    RejectCodes.MinFee,
    "staticLedgerRules",
    fromNativeTx({ fee: 0n }),
    configFor({ minFeeB: 1n }),
  ),
  evidenceCase(
    "required signer without a witness",
    RejectCodes.MissingRequiredWitness,
    "signatures",
    fromNativeTx({ requiredSignerItems: [Buffer.alloc(28, 0x5a)] }),
  ),
  evidenceCase(
    "vkey witness signs the wrong body hash",
    RejectCodes.InvalidSignature,
    "signatures",
    fromNativeTx({ invalidVkeyWitness: true }),
  ),
  evidenceCase(
    "native script requires an absent signer",
    RejectCodes.NativeScriptInvalid,
    "phaseANativeScripts",
    fromNativeTx({
      scriptWitnesses: [
        nativeScriptWitness({ type: "sig", keyHash: Buffer.alloc(28, 0x33) }),
      ],
    }),
  ),
  evidenceCase(
    "admission of a non-valid transaction",
    RejectCodes.IsValidFalseForbidden,
    "canonicalDecode",
    fromNativeTx({ validity: "TxIsInvalid" }),
  ),
  evidenceCase(
    "auxiliary data hash present",
    RejectCodes.AuxDataForbidden,
    "canonicalDecode",
    fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) }),
  ),
  evidenceCase(
    "network id differs from the header-committed one",
    RejectCodes.NetworkIdMismatch,
    "staticLedgerRules",
    fromNativeTx({ networkId: 1n }),
  ),
  evidenceCase(
    "consensus profile is not the compiled V1 tuple",
    RejectCodes.TxVersion,
    "canonicalDecode",
    fromNativeTx({}),
    {
      ...CONFIG,
      consensusProfile: {
        ...MIDGARD_CONSENSUS_PROFILE,
        protocolVersion: 2,
      } as unknown as typeof MIDGARD_CONSENSUS_PROFILE,
    },
  ),
  evidenceCase(
    "canonical transaction over the V1 size bound",
    RejectCodes.TxSize,
    "canonicalDecode",
    fromNativeTx({ outputs: manyOutputs(8000) }),
  ),
  evidenceCase(
    "single output value over the Cardano value bound",
    RejectCodes.ValueSize,
    "canonicalDecode",
    fromNativeTx({
      outputs: [makeOutput(1n, TEST_ADDRESS_BYTES, assetMap(2600))],
    }),
  ),
  evidenceCase(
    "outputs preimage over the field bound",
    RejectCodes.FieldPreimageSize,
    "canonicalDecode",
    fromNativeTx({ outputs: manyOutputs(2000) }),
  ),
  evidenceCase(
    "plutus witness is not a canonical bounded program envelope",
    RejectCodes.ScriptProgramEncoding,
    "canonicalDecode",
    fromNativeTx({
      scriptWitnesses: [plutusV3ScriptWitness(Buffer.from([0x00]))],
    }),
  ),
  evidenceCase(
    "missing program-material sidecar",
    RejectCodes.CekProgramMaterial,
    "canonicalDecode",
    fromNativeTx({}, { programMaterialSidecarCbor: null }),
  ),
];

/** Valid transactions that must be accepted, used by the differential. */
export const VALID_CASES: readonly QueuedTx[] = [
  fromNativeTx({}),
  fromNativeTx({ outputs: [makeOutput(1n), makeOutput(2n)] }),
  fromNativeTx({ referenceInputs: [outRefFromByte(0x72)] }),
  fromNativeTx({ validityIntervalStart: 1n, validityIntervalEnd: 9n }),
  fromNativeTx({
    scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 0n })],
  }),
];

// ---------------------------------------------------------------------------
// Block fixture
// ---------------------------------------------------------------------------

export const headerHashOf = (header: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(header, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

export const sortEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const bufferEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly { readonly key: Buffer; readonly value: Buffer }[] =>
  entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export const hex = <A>(
  value: A,
  schema: Parameters<typeof Data.to>[1],
): string => encodeData(value, schema as never).toString("hex");

export const watcherHeaderRecord = (
  header: SDK.Header,
  headerHash: string,
): WatcherStateQueueHeader => ({
  headerHash,
  headerCborHex: Data.to(header, SDK.Header),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: header.prevUtxosRoot,
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
  withdrawalCount: header.withdrawalCount.toString(),
  forcedTransactionCount: header.forcedTransactionCount.toString(),
  l2TransactionCount: header.l2TransactionCount.toString(),
  depositCount: header.depositCount.toString(),
  totalEventCount: header.totalEventCount.toString(),
  transitionStepCount: header.transitionStepCount.toString(),
  validationTraceCount: header.validationTraceCount.toString(),
  startTime: header.startTime.toString(),
  endTime: header.endTime.toString(),
  blockSlot: header.blockSlot.toString(),
  expectedNetworkId: header.expectedNetworkId.toString(),
  minFeeA: header.minFeeA.toString(),
  minFeeB: header.minFeeB.toString(),
  prevHeaderHash: header.prevHeaderHash,
  operatorVkey: header.operatorVkey,
  protocolVersion: header.protocolVersion.toString(),
  daAttestationPolicyId: null,
});

export type BlockFixture = {
  readonly payload: SDK.DaPayload;
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly envelope: Buffer;
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly reconstruction: WatcherHeaderRootReconstructionResult;
  readonly txCbors: readonly Buffer[];
};
