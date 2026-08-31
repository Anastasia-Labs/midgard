/**
 * Shared real-contract emulator fixtures for the `mint-authorization` family.
 *
 * The family convicts an operator-ACCEPTED committed transaction that mints
 * under a policy whose native script is either absent from the transaction's
 * machine-consulted source surface (direction A) or present-but-unsatisfied
 * against the committed signer set and validity interval (direction B).
 *
 * What every scenario needs and no existing helper produces is a committed
 * transaction with caller-chosen §2.5 field-5 (mint), field-6 (script
 * witnesses), field-7 (address witnesses) and field-1 (reference inputs)
 * preimages, materialised directly from canonical bytes so the §8.8 field
 * doors the five steps open reproduce the committed commitments exactly.
 *
 * The subject preimages of the four doors the steps consume are always spelt
 * with {@link encodeMidgardFieldPreimageV1} — byte-for-byte what the planner
 * re-envelopes — so a door's carriage tier is only ever selected by the
 * preimage's own length, never forced.
 */
import {
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardMintPolicyItemV1,
  encodeMidgardNativeScript,
  encodeMidgardSpendInputItemV1,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCarriageBoundsV1,
  type MidgardMintPolicyItemV1,
  type MidgardNativeScript,
  type MidgardNativeTxFullV1,
  sortMidgardMintItemsV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";

import type { MintAuthorizationContractsV1 } from "../../src/mint-authorization/contracts-v1.js";
import { buildMintAuthorizationStep02EvidenceV1 } from "../../src/mint-authorization/evidence-v1.js";
import {
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "../../src/mint-authorization/submit-common-v1.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import {
  excludeUtxo,
  minimumLovelaceForInlineDatumOutput,
  resolveProtocolParameters,
} from "../../src/spend-input-witness.js";
import { selectFeeInput } from "../../src/submit-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../../src/witness-reference-scripts-v1.js";
import { publishFaultProofWitnessReferenceScriptsV1 } from "./emulator/reference-scripts.js";
import {
  buildDecodingBlockFixtureV1,
  buildDecodingLedgerFixtureV1,
  type DecodingBlockFixtureV1,
  type DecodingLedgerFixtureV1,
} from "./native-script-decoding-emulator-v1.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  funderPaymentKeyHash,
  makeFaultProofEmulatorHarnessV1,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";

export { expectOnchainRefusalV1 } from "./native-script-decoding-emulator-v1.js";

// ---------------------------------------------------------------------------
// §5.6 mint items and §5.5 native scripts
// ---------------------------------------------------------------------------

/** One §5.6 single-asset mint policy item's canonical bytes, hex. */
export const mintItemCborV1 = ({
  policyId,
  assetName,
  quantity = 1n,
}: {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity?: bigint;
}): string =>
  Buffer.from(
    encodeMidgardMintPolicyItemV1({
      policyId,
      assets: [{ assetName, quantity }],
    }),
  ).toString("hex");

/** A readable 28-byte policy id: `byte` repeated. */
export const policyIdByteV1 = (byte: number): Buffer => Buffer.alloc(28, byte);

/** A distinct ascending 28-byte policy id: `index` in the trailing four bytes. */
const ascendingPolicyIdV1 = (index: number): Buffer => {
  const buffer = Buffer.alloc(28);
  buffer.writeUInt32BE(index >>> 0, 24);
  return buffer;
};

/**
 * The direction-B subject: a native `sig` timelock whose key hash signs no
 * committed witness, so its machine-twin evaluation over the (empty) field-7
 * signer set is unsatisfied. Returns the policy id it hashes to, the mint
 * item that spends under it, and the canonical payload bytes step-03 pins.
 */
export const directionBNativeScriptV1 = (
  keyByte = 0xcd,
): {
  readonly script: MidgardNativeScript;
  readonly scriptBytesHex: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
} => {
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, keyByte),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const policyIdHex = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript: script,
  });
  return {
    script,
    scriptBytesHex: scriptBytes.toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
  };
};

/**
 * A direction-B native `sig` whose key hash IS a committed signer — the honest
 * false-fault: its machine-twin evaluates SATISFIED, so no fault exists. Used
 * to drive step-03's EvaluateUnsatisfied local refusal.
 */
export const directionBSatisfiedNativeScriptV1 = (): {
  readonly scriptBytesHex: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
  readonly addrWitnessItemCbor: string;
} => {
  const verificationKey = Buffer.alloc(32, 0x07);
  const keyHashHex = Effect.runSync(
    SDK.hashHexWithBlake2b(verificationKey.toString("hex"), 28),
  );
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.from(keyHashHex, "hex"),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const policyIdHex = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes,
    nativeScript: script,
  });
  return {
    scriptBytesHex: scriptBytes.toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
    addrWitnessItemCbor: Buffer.from(
      encodeMidgardAddressWitnessItemV1({
        verificationKey,
        signature: Buffer.alloc(64, 0x09),
      }),
    ).toString("hex"),
  };
};

/**
 * A field-6 versioned-script item whose hash IS a claimed policy id — the
 * honest false-fault for direction A: a script the operator DID consult, so
 * the absence claim is false. Returns the item bytes and the policy id.
 */
export const directionAPresentScriptV1 = (): {
  readonly scriptWitnessItemCbor: string;
  readonly policyIdHex: string;
  readonly mintItemCbor: string;
} => {
  const script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, 0x5e),
  };
  const scriptBytes = encodeMidgardNativeScript(script);
  const versionedScript = {
    language: "NativeCardano" as const,
    scriptBytes,
    nativeScript: script,
  };
  const policyIdHex = hashMidgardVersionedScript(versionedScript);
  return {
    scriptWitnessItemCbor:
      encodeMidgardVersionedScript(versionedScript).toString("hex"),
    policyIdHex,
    mintItemCbor: mintItemCborV1({
      policyId: Buffer.from(policyIdHex, "hex"),
      assetName: Buffer.from("beef", "hex"),
    }),
  };
};

/** A small single-policy tier-1 mint field: one absent policy, one asset. */
export const smallMintItemCborsV1 = (): readonly string[] => [
  mintItemCborV1({
    policyId: policyIdByteV1(0xab),
    assetName: Buffer.from("beef", "hex"),
  }),
];

/**
 * A mint field whose §5.1 preimage lands in the tier-2 RawUtxo window
 * `(maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes]`, forced by
 * item count alone: distinct ascending single-asset policies, 32-byte asset
 * names, computed at build time from the encoded bytes rather than hard-coded.
 * Every policy is absent, so the whole field is direction-A convictable.
 */
export const largeMintItemCborsV1 = (): {
  readonly itemCbors: readonly string[];
  readonly preimageByteLength: number;
} => {
  const { maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes } =
    midgardFieldCarriageBoundsV1;
  const assetName = Buffer.alloc(32, 0x11);
  const items: MidgardMintPolicyItemV1[] = [];
  let count = 0;
  let preimageByteLength = 0;
  // Grow the field one policy at a time until its own preimage crosses the
  // tier-1 ceiling; the loop stops the instant tier-2 is selected by size.
  for (;;) {
    items.push({
      policyId: ascendingPolicyIdV1(count),
      assets: [{ assetName, quantity: 1n }],
    });
    count += 1;
    const sorted = sortMidgardMintItemsV1(items);
    const itemBuffers = sorted.map((item) =>
      Buffer.from(encodeMidgardMintPolicyItemV1(item)),
    );
    preimageByteLength = encodeMidgardFieldPreimageV1(itemBuffers).length;
    if (preimageByteLength > maxTier1RedeemerPreimageBytes) {
      if (preimageByteLength > maxPublishableCarriageBytes) {
        throw new Error(
          `large mint field overshot the single-publication window: ${preimageByteLength.toString()} bytes`,
        );
      }
      return {
        itemCbors: itemBuffers.map((item) => item.toString("hex")),
        preimageByteLength,
      };
    }
  }
};

/** N §5.3 address-witness items (fixed 101 bytes each), distinct decoy vkeys. */
export const addressWitnessItemCborsV1 = (count: number): readonly string[] =>
  Array.from({ length: count }, (_unused, index) => {
    const verificationKey = Buffer.alloc(32);
    verificationKey.writeUInt32BE(index + 1, 28);
    return Buffer.from(
      encodeMidgardAddressWitnessItemV1({
        verificationKey,
        signature: Buffer.alloc(64, (index % 254) + 1),
      }),
    ).toString("hex");
  });

/**
 * A field-7 address-witness set whose §5.1 preimage crosses the tier-2 window
 * (103-byte per-item stride), forced by count alone. None of the decoy signers
 * matches a direction-B `sig` key hash, so the policy stays unsatisfied.
 */
export const largeAddressWitnessItemCborsV1 = (): {
  readonly itemCbors: readonly string[];
  readonly preimageByteLength: number;
} => {
  const { maxTier1RedeemerPreimageBytes, maxPublishableCarriageBytes } =
    midgardFieldCarriageBoundsV1;
  let count = 0;
  for (;;) {
    count += 1;
    const itemCbors = addressWitnessItemCborsV1(count);
    const preimageByteLength = encodeMidgardFieldPreimageV1(
      itemCbors.map((hex) => Buffer.from(hex, "hex")),
    ).length;
    if (preimageByteLength > maxTier1RedeemerPreimageBytes) {
      if (preimageByteLength > maxPublishableCarriageBytes) {
        throw new Error(
          `large address-witness field overshot the single-publication window: ${preimageByteLength.toString()} bytes`,
        );
      }
      return { itemCbors, preimageByteLength };
    }
  }
};

/** One §5.3 reference-input out-ref item's canonical bytes, hex. */
export const referenceInputItemCborV1 = ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): string =>
  Buffer.from(
    encodeMidgardSpendInputItemV1({
      txId: Buffer.from(txIdHex, "hex"),
      outputIndex,
    }),
  ).toString("hex");

// ---------------------------------------------------------------------------
// The committed transaction and its block fixture
// ---------------------------------------------------------------------------

const fieldPreimageOf = (itemCbors: readonly string[]): Buffer =>
  itemCbors.length === 0
    ? Buffer.from(EMPTY_CBOR_LIST)
    : encodeMidgardFieldPreimageV1(
        itemCbors.map((hex) => Buffer.from(hex, "hex")),
      );

export type MintAuthorizationSubjectV1 = {
  readonly nativeTx: MidgardNativeTxFullV1;
  readonly witnessSetCompact: SDK.NativeTxWitnessSetCompact;
  readonly mintItemCbors: readonly string[];
  readonly scriptWitnessItemCbors: readonly string[];
  readonly addrWitnessItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
};

/**
 * Materialises the committed native transaction from its four door preimages.
 * `validity: "TxIsInvalid"` builds the §2.4.3(d) negative — an honestly
 * recorded no-op the family must never convict.
 */
export const buildMintAuthorizationSubjectV1 = ({
  mintItemCbors,
  scriptWitnessItemCbors = [],
  addrWitnessItemCbors = [],
  referenceInputItemCbors = [],
  validity = "TxIsValid",
}: {
  readonly mintItemCbors: readonly string[];
  readonly scriptWitnessItemCbors?: readonly string[];
  readonly addrWitnessItemCbors?: readonly string[];
  readonly referenceInputItemCbors?: readonly string[];
  readonly validity?: "TxIsValid" | "TxIsInvalid";
}): MintAuthorizationSubjectV1 => {
  const nativeTx = materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity,
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: fieldPreimageOf(referenceInputItemCbors),
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: fieldPreimageOf(mintItemCbors),
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: fieldPreimageOf(addrWitnessItemCbors),
      scriptTxWitsPreimageCbor: fieldPreimageOf(scriptWitnessItemCbors),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(nativeTx.witnessSet);
  return {
    nativeTx,
    witnessSetCompact: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
    mintItemCbors,
    scriptWitnessItemCbors,
    addrWitnessItemCbors,
    referenceInputItemCbors,
  };
};

// ---------------------------------------------------------------------------
// Harness, committed header, reference scripts, removal category
// ---------------------------------------------------------------------------

export const makeMintAuthorizationEmulatorHarnessV1 = async ({
  useScalusEvaluator = true,
}: {
  /**
   * Scalus avoids the Aiken/WASM arena leak on multi-step lifecycles. Small
   * adversarial tests may opt out so the legacy shared refusal guard observes
   * the emulator's canonical `failed script execution` submission error.
   */
  readonly useScalusEvaluator?: boolean;
} = {}) => {
  const harness = await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realMintAuthorization: true,
      alwaysFraudProofCatalogue: true,
    },
    ...(useScalusEvaluator
      ? { lucidOptions: { evaluator: createScalusEvaluator() } }
      : {}),
  });
  const family = harness.contracts.mintAuthorization;
  const category = harness.catalogue.categories.mintAuthorization;
  if (family === undefined || category === undefined) {
    throw new Error(
      "Harness did not build the mint-authorization contracts/category",
    );
  }
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.mintAuthorization
  ) {
    throw new Error("Unexpected mint-authorization catalogue category id");
  }
  return { ...harness, family, category };
};

export type MintAuthorizationHarnessV1 = Awaited<
  ReturnType<typeof makeMintAuthorizationEmulatorHarnessV1>
>;

export type MintAuthorizationScenarioV1 = {
  readonly subject: MintAuthorizationSubjectV1;
  readonly block: DecodingBlockFixtureV1;
  readonly setup: Awaited<ReturnType<typeof submitSetupTx>> & {
    readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  };
};

/**
 * Commits a header whose transition trace carries the accused transaction as
 * an accepted L2 event and its dense transition step, ready for Init.
 */
export const setupMintAuthorizationScenarioV1 = async ({
  harness,
  subject,
  priorLedgerRoot = SDK.EMPTY_MERKLE_TREE_ROOT,
}: {
  readonly harness: MintAuthorizationHarnessV1;
  readonly subject: MintAuthorizationSubjectV1;
  /** The block's pre-state ledger root; the step-04 ResolveNext trie root. */
  readonly priorLedgerRoot?: string;
}): Promise<MintAuthorizationScenarioV1> => {
  const witnessReferenceScripts =
    await publishFaultProofWitnessReferenceScriptsV1({
      lucid: harness.proverLucid,
      realBlueprint: harness.realBlueprint,
      claimRegistrySpendingScript:
        harness.contracts.claimRegistry.spendingScript,
      computationThreadMintingScript:
        harness.family.computationThread.mintingScript,
      fraudProofMintingScript: harness.family.fraudProof.mintingScript,
    });
  const operatorVkey = await funderPaymentKeyHash(harness.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      harness.funderLucid,
      harness.emulator.now() + 120_000,
    ) - 1,
  );
  const block = await buildDecodingBlockFixtureV1({
    operatorVkey,
    startTime,
    priorLedgerRoot,
    subject: { kind: "normal", nativeTx: subject.nativeTx },
  });
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue: harness.catalogue,
    header: block.header,
  });
  return {
    subject,
    block,
    setup: { ...setup, witnessReferenceScripts },
  };
};

/** Publishes all five step validators as reference scripts (deployment shape). */
export const publishMintAuthorizationReferenceScriptsV1 = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Parameters<
    typeof publishPlainReferenceScriptUtxo
  >[0]["lucid"];
  readonly contracts: MintAuthorizationContractsV1;
}): Promise<readonly [UTxO, UTxO, UTxO, UTxO, UTxO]> => {
  const published: UTxO[] = [];
  for (const [index, step] of contracts.steps.entries()) {
    const script: Script = step.spendingScript;
    const { utxo } = await publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `mint-authorization step-0${(index + 1).toString()}`,
      oversized: true,
    });
    published.push(utxo);
  }
  return published as unknown as readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
};

// ---------------------------------------------------------------------------
// The step-04 ResolveNext pre-state ledger trie (one reference input)
// ---------------------------------------------------------------------------

/**
 * A pre-state ledger trie holding one scanned outpoint's descriptor under its
 * §5.3 38-byte key, its reference script hashing to something OTHER than the
 * accused policy so the absence claim holds. The block's `priorLedgerRoot`
 * must be set to the returned `rootHex`.
 */
export const buildMintAuthorizationLedgerFixtureV1 = async ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): Promise<DecodingLedgerFixtureV1> =>
  buildDecodingLedgerFixtureV1({
    txIdHex,
    outputIndex,
    referenceScriptItemBytes: Buffer.from("a1b2c3d4", "hex"),
    referenceScriptLanguage: 0,
  });

// ---------------------------------------------------------------------------
// Raw guard-bypassing builder — a step-02 mint-door open against TAMPERED
// predeployed carriage, so the on-chain §8.8 commitment re-hash refuses. The
// honest builders locate publications BY CONTENT and can never reach tampered
// bytes, so the adversarial path constructs the RawUtxo opening directly.
// ---------------------------------------------------------------------------

/** Flips one byte of a field preimage — a publication the door must reject. */
export const tamperFieldPreimageBytesV1 = (preimage: Buffer): Buffer => {
  const tampered = Buffer.from(preimage);
  const index = tampered.length - 1;
  tampered[index] = tampered[index]! ^ 0xff;
  return tampered;
};

/**
 * Publishes a bytes-only inline-datum UTxO carrying `bytes` at the prover's
 * own address, in the exact §8 publication datum shape the door reads, and
 * returns the confirmed UTxO. Used only to plant TAMPERED carriage.
 */
export const publishRawFieldPreimageCarriageV1 = async ({
  lucid,
  signer,
  bytes,
}: {
  readonly lucid: MintAuthorizationHarnessV1["proverLucid"];
  readonly signer: ResolvedProverSigner;
  readonly bytes: Buffer;
}): Promise<UTxO> => {
  signer.selectWallet(lucid);
  const datum = SDK.fieldPreimagePublicationDatumCborV1(bytes);
  const { coinsPerUtxoByte } = await resolveProtocolParameters(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .pay.ToAddressWithData(
      signer.address,
      { kind: "inline", value: datum },
      {
        lovelace: minimumLovelaceForInlineDatumOutput({
          address: signer.address,
          datum,
          coinsPerUtxoByte,
        }),
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const published = (await lucid.utxosAt(signer.address)).find(
    (utxo) => utxo.datum === datum,
  );
  if (published === undefined) {
    throw new Error("raw tampered publication did not land at prover address");
  }
  return published;
};

/**
 * The honest step-02 transaction shape with the mint door opened against a
 * caller-planted TAMPERED tier-2 publication. Every local twin is omitted, so
 * the recomputed §8.8 field commitment reaches the validator, which aborts
 * when it disagrees with the anchored committed slot.
 */
export const submitRawMintAuthorizationStep02TamperedMintV1 = async ({
  harness,
  threadOutRef,
  block,
  tamperedPreimageBytes,
  referenceScriptUtxo,
}: {
  readonly harness: MintAuthorizationHarnessV1;
  readonly threadOutRef: string;
  readonly block: DecodingBlockFixtureV1;
  readonly tamperedPreimageBytes: Buffer;
  readonly referenceScriptUtxo: UTxO;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const anchorState = requireMintAuthorizationStepStateV1({
    threadUtxo,
    signer,
    schema: SDK.MintAuthorizationStep02Datum,
    stepIndex: 1,
  });
  const evidence = await buildMintAuthorizationStep02EvidenceV1({
    reconstruction: block.reconstruction,
    eventKey: { L2TransactionEventKey: { tx_id: anchorState.bad_tx_id } },
  });
  const tamperedUtxo = await publishRawFieldPreimageCarriageV1({
    lucid,
    signer,
    bytes: tamperedPreimageBytes,
  });
  const referenceInputs = [
    tamperedUtxo,
    requireMintAuthorizationReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[1].spendingScriptHash,
      stepIndex: 1,
    }),
  ];
  const step03State: SDK.MintAuthorizationStep03StateV1 = {
    policy_id: "ab".repeat(28),
    direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
    bad_tx_id: anchorState.bad_tx_id,
    bad_tx_witness_set_hash: anchorState.bad_tx_witness_set_hash,
    validity_interval_start: anchorState.validity_interval_start,
    validity_interval_end: anchorState.validity_interval_end,
    prior_ledger_root: evidence.transitionStepMembership.value.pre_utxos_root,
  };
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    SDK.MintAuthorizationStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  signer.selectWallet(lucid);
  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = excludeUtxo(walletUtxos, tamperedUtxo);
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "raw mint-authorization step-02",
    );
    const refIndex = SDK.requireReferenceInputIndex(
      ctx,
      tamperedUtxo,
      "raw mint-authorization tampered publication",
    );
    const mintOpening = SDK.fieldOpeningV1ForField({
      fieldIndex: SDK.MIDGARD_FIELD_INDEX_V1.mint,
      nativeTxCompactCbor: block.nativeTxCompactCbor,
      carriage: { RawUtxo: { ref_input_index: refIndex } },
    });
    return Data.to(
      {
        Continue: [
          {
            input_index: SDK.requireInputIndex(
              ctx,
              threadUtxo,
              "raw mint-authorization step-02",
            ),
            output_index: SDK.requireUniqueOutputIndex(
              ctx.outputs,
              step03OutputMatches,
              "raw mint-authorization step-02 output",
            ),
            header: block.reconstruction.header,
            event_to_step_membership: evidence.eventToStepMembership,
            transition_step_membership: evidence.transitionStepMembership,
            policy_index: 0n,
            direction: SDK.MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
            mint_opening: mintOpening,
          },
        ],
      },
      SDK.MintAuthorizationStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({
      localUPLCEval: true,
      presetWalletInputs: walletUtxosSansCarriage as UTxO[],
    });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

export { network };
