import "./native-transaction-integration.make-mint-preimage.js";

import {
  encodeMidgardCekProgramMaterialSidecar,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeHash32,
  computeMidgardNativeTxId,
  computeScriptIntegrityHashForLanguages,
  deriveMidgardNativeTxBodyCompact,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  type ScriptLanguageName,
  ScriptLanguageTags,
} from "@al-ft/midgard-core/codec";
import {
  type QueuedTx,
  RejectCodes,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";
import {
  canonicalizeTestProofScript,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  encodeByteList,
  makeOutRef,
  type ScriptWitnessItem,
  scriptWitnessItemToVersioned,
  TEST_ADDRESS,
  testProgramMaterial,
} from "./native-transaction-integration.script-witness-item-to-versioned.js";

export const buildNativeTx = (opts?: {
  readonly redeemerTxWitsPreimageCbor?: Buffer;
  readonly requiredObserverItems?: readonly Uint8Array[];
  readonly scriptWitnessItems?: readonly ScriptWitnessItem[];
  readonly witnessMode?: "none" | "valid" | "invalid";
  readonly witnessSignerPrivateKey?: CML.PrivateKey;
  readonly mintPreimageCbor?: Buffer;
  readonly networkId?: bigint;
  readonly outputCount?: number;
  readonly outputCbors?: readonly Buffer[];
  readonly scriptIntegrityHash?: Buffer;
  readonly scriptLanguages?: readonly ScriptLanguageName[];
  readonly version?: bigint;
  readonly spendInputOutRefs?: readonly Buffer[];
  readonly referenceInputOutRefs?: readonly Buffer[];
}): {
  tx: MidgardNativeTxFull;
  txId: Buffer;
  txCbor: Buffer;
  inputOutRef: Buffer;
  referenceInputOutRef: Buffer;
  outputCbor: Buffer;
} => {
  const spendInputs = opts?.spendInputOutRefs ?? [makeOutRef(0x11, 0n)];
  const referenceInputs = opts?.referenceInputOutRefs ?? [makeOutRef(0x22, 1n)];
  const spendInput = spendInputs[0];
  const referenceInput = referenceInputs[0];
  const defaultOutput = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(TEST_ADDRESS),
      CML.Value.from_coin(3_000_000n),
    ).to_cbor_bytes(),
  );
  const outputCount = Math.max(1, opts?.outputCount ?? 1);
  const outputCbors =
    opts?.outputCbors !== undefined
      ? [...opts.outputCbors]
      : Array.from({ length: outputCount }, () => Buffer.from(defaultOutput));
  const outputCbor = outputCbors[0];

  const spendInputsPreimageCbor = encodeByteList(spendInputs);
  const referenceInputsPreimageCbor = encodeByteList(referenceInputs);
  const outputsPreimageCbor = encodeByteList(outputCbors);
  const requiredObserversPreimageCbor = encodeByteList(
    opts?.requiredObserverItems ?? [],
  );
  const witnessMode = opts?.witnessMode ?? "none";
  const witnessSignerPrivateKey =
    witnessMode === "none"
      ? undefined
      : (opts?.witnessSignerPrivateKey ?? CML.PrivateKey.generate_ed25519());
  const requiredSignersPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(
            witnessSignerPrivateKey.to_public().hash().to_raw_bytes(),
          ),
        ]);
  const mintPreimageCbor = opts?.mintPreimageCbor ?? EMPTY_CBOR_LIST;

  const scriptTxWitsPreimageCbor =
    opts?.scriptWitnessItems === undefined
      ? EMPTY_CBOR_LIST
      : encodeMidgardVersionedScriptListPreimage(
          opts.scriptWitnessItems
            .map(scriptWitnessItemToVersioned)
            .map(canonicalizeTestProofScript),
        );
  const redeemerTxWitsPreimageCbor =
    opts?.redeemerTxWitsPreimageCbor ?? EMPTY_CBOR_LIST;
  const scriptIntegrityHash =
    opts?.scriptIntegrityHash ??
    (opts?.scriptLanguages !== undefined
      ? computeScriptIntegrityHashForLanguages(
          deriveMidgardNativeTxWitnessSetCompact({
            addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
            scriptTxWitsPreimageCbor,
            redeemerTxWitsPreimageCbor,
          }).redeemerTxWitsHash,
          opts.scriptLanguages,
        )
      : computeHash32(EMPTY_CBOR_NULL));
  const version = opts?.version ?? MIDGARD_NATIVE_TX_VERSION;

  const body: MidgardNativeTxBodyCanonical = {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash,
    auxiliaryDataHash: computeHash32(EMPTY_CBOR_NULL),
    networkId: opts?.networkId ?? 0n,
  };

  const signedBodyHash =
    witnessMode === "invalid"
      ? Buffer.alloc(32, 0x7f)
      : computeMidgardNativeTxId({
          version,
          transactionBody: deriveMidgardNativeTxBodyCompact(body),
          transactionWitnessSetHash: Buffer.alloc(32),
          validity: "TxIsValid",
        });
  const addrTxWitsPreimageCbor =
    witnessSignerPrivateKey === undefined
      ? EMPTY_CBOR_LIST
      : encodeByteList([
          Buffer.from(
            CML.make_vkey_witness(
              CML.TransactionHash.from_raw_bytes(signedBodyHash),
              witnessSignerPrivateKey,
            ).to_cbor_bytes(),
          ),
        ]);

  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };

  const tx: MidgardNativeTxFull = {
    version,
    validity: "TxIsValid",
    compact: deriveMidgardNativeTxCompact(
      body,
      witnessSet,
      "TxIsValid",
      version,
    ),
    body,
    witnessSet,
  };

  const txCbor = encodeMidgardNativeTxCanonical(tx);
  const txId = computeMidgardNativeTxId(tx);

  return {
    tx,
    txId,
    txCbor,
    inputOutRef: spendInput,
    referenceInputOutRef: referenceInput,
    outputCbor,
  };
};

export const attachComputedScriptIntegrityHash = (
  fixture: ReturnType<typeof buildNativeTx>,
  usedLanguages: readonly (number | ScriptLanguageName)[],
): ReturnType<typeof buildNativeTx> => {
  const languages = usedLanguages.map((language): ScriptLanguageName => {
    if (language === CML.Language.PlutusV3) {
      return "PlutusV3";
    }
    if (language === ScriptLanguageTags.MidgardV1) {
      return "MidgardV1";
    }
    if (language === "PlutusV3" || language === "MidgardV1") {
      return language;
    }
    throw new Error(`unsupported script language in test: ${String(language)}`);
  });

  const scriptIntegrityHash = computeScriptIntegrityHashForLanguages(
    deriveMidgardNativeTxWitnessSetCompact(fixture.tx.witnessSet)
      .redeemerTxWitsHash,
    languages,
  );

  const body: MidgardNativeTxBodyCanonical = {
    ...fixture.tx.body,
    scriptIntegrityHash,
  };
  const tx: MidgardNativeTxFull = {
    ...fixture.tx,
    body,
    compact: deriveMidgardNativeTxCompact(
      body,
      fixture.tx.witnessSet,
      fixture.tx.validity,
      fixture.tx.version,
    ),
  };

  return {
    ...fixture,
    tx,
    txId: computeMidgardNativeTxId(tx),
    txCbor: encodeMidgardNativeTxCanonical(tx),
  };
};

export const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase1_midgard",
} as const;

export const mkQueued = (
  txId: Buffer,
  txCbor: Buffer,
  programMaterial?: readonly MidgardCekProgramMaterialEntry[],
): QueuedTx => ({
  txId,
  txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([
    ...(programMaterial ?? testProgramMaterial.values()),
  ]),
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

export const runBothPhases = async (
  txId: Buffer,
  txCbor: Buffer,
  preState: Map<string, Buffer>,
  phaseBOptions?: {
    readonly enforceScriptBudget?: boolean;
  },
  programMaterial?: readonly MidgardCekProgramMaterialEntry[],
) => {
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(
      [mkQueued(txId, txCbor, programMaterial)],
      phaseAConfig,
    ),
  );
  const { accepted, rejected } = await Effect.runPromise(
    runPhaseBValidationWithPatch(phaseA.accepted, preState, {
      nowCardanoSlotNo: 0n,
      bucketConcurrency: 1,
      ...phaseBOptions,
    }),
  );
  return { phaseA, phaseB: { accepted, rejected } };
};

type BothPhaseResult = Awaited<ReturnType<typeof runBothPhases>>;

type PhaseAResult = BothPhaseResult["phaseA"];

type PhaseBResult = BothPhaseResult["phaseB"];

type RejectCode = (typeof RejectCodes)[keyof typeof RejectCodes];

export const expectPhaseAAcceptsOne = (phaseA: PhaseAResult) => {
  expect(
    phaseA.rejected,
    JSON.stringify(phaseA.rejected, null, 2),
  ).toHaveLength(0);
  expect(phaseA.accepted).toHaveLength(1);
};

const expectPhaseBAcceptsOne = (phaseB: PhaseBResult) => {
  expect(
    phaseB.rejected,
    JSON.stringify(phaseB.rejected, null, 2),
  ).toHaveLength(0);
  expect(phaseB.accepted).toHaveLength(1);
};

export const expectBothPhasesAcceptOne = ({
  phaseA,
  phaseB,
}: BothPhaseResult) => {
  expectPhaseAAcceptsOne(phaseA);
  expectPhaseBAcceptsOne(phaseB);
};

export const expectPhaseBRejectsOne = (
  phaseB: PhaseBResult,
  code: RejectCode,
  detail?: string,
) => {
  expect(phaseB.accepted).toHaveLength(0);
  expect(phaseB.rejected).toHaveLength(1);
  expect(phaseB.rejected[0].code).toBe(code);
  if (detail !== undefined) {
    expect(phaseB.rejected[0].detail).toContain(detail);
  }
};

export const expectPhaseAAcceptsAndPhaseBRejectsOne = (
  result: BothPhaseResult,
  code: RejectCode,
  detail?: string,
) => {
  expectPhaseAAcceptsOne(result.phaseA);
  expectPhaseBRejectsOne(result.phaseB, code, detail);
};

export type BaseLedgerOutRefs = {
  readonly inputOutRef: Buffer;
  readonly referenceInputOutRef: Buffer;
};
