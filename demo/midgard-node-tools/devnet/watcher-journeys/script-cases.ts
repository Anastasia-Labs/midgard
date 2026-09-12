import {
  computeScriptIntegrityHashForLanguages,
  decodeMidgardAddressBytes,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  hashMidgardVersionedScript,
  midgardFieldCommitment,
  protectMidgardAddress,
} from "@al-ft/midgard-core";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { retainedTransactionFixture } from "@al-ft/midgard-fault-proofs/test-support/retained-transaction";
import type * as SDK from "@al-ft/midgard-sdk";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation";
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

const identityProgram = buildMidgardCanonicalCekProgram(
  Buffer.from("010100200101", "hex"),
);

/** An executable, signed transaction for real forced-order verdict disputes. */
export const buildJourneyScriptTransaction = (input: {
  predecessor: { header: SDK.Header; payload: SDK.DaPayload };
  ledgerOwnerSeedPhrase: string;
  variant?: "plutusMint" | "nativeMint" | "nativeReceive";
  outputReferenceScript?: boolean;
  retainWalletChange?: boolean;
}) => {
  const wallet = walletFromSeed(input.ledgerOwnerSeedPhrase, {
    network: "Custom",
  });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const spent = input.predecessor.payload.block_body.utxos.find(([, bytes]) => {
    const credential = decodeMidgardAddressBytes(
      decodeMidgardTxOutput(Buffer.from(bytes, "hex")).address,
    ).paymentCredential;
    return (
      credential.kind === "PubKey" && Buffer.from(credential.hash).equals(owner)
    );
  });
  if (spent === undefined)
    throw new Error("Script journey needs a retained ledger input");
  const output = decodeMidgardTxOutput(Buffer.from(spent[1], "hex"));
  const variant = input.variant ?? "plutusMint";
  const script =
    variant === "plutusMint"
      ? {
          language: "PlutusV3" as const,
          scriptBytes: identityProgram.envelopeCbor,
        }
      : decodeMidgardVersionedScript(Buffer.from("820043820180", "hex"));
  const policy = hashMidgardVersionedScript(script);
  const assets = new Map(
    [...output.value.assets].map(([hash, tokens]) => [hash, new Map(tokens)]),
  );
  const tokens = assets.get(policy) ?? new Map<string, bigint>();
  if (variant !== "nativeReceive") {
    tokens.set("", (tokens.get("") ?? 0n) + 1n);
    assets.set(policy, tokens);
  }
  const mintRedeemer = encodeMidgardRedeemerWitnessItem({
    purpose: "Mint",
    index: 0n,
    redeemerCbor: Buffer.from("00", "hex"),
    executionUnits: { memory: 1_000_000_000n, steps: 1_000_000_000n },
  });
  const scriptOutput = {
    ...output,
    address:
      variant === "nativeReceive"
        ? Buffer.concat([Buffer.from([0x78]), Buffer.from(policy, "hex")])
        : protectMidgardAddress(output.address),
    value: { ...output.value, assets },
    script_ref:
      input.outputReferenceScript === false
        ? undefined
        : decodeMidgardVersionedScript(Buffer.from("820043820180", "hex")),
  };
  const outputs = [encodeMidgardTxOutput(scriptOutput)];
  if (input.retainWalletChange) {
    const receiveLovelace = 3_000_000n;
    if (
      variant !== "nativeReceive" ||
      output.value.lovelace < 2n * receiveLovelace
    )
      throw new Error(
        "Native receive journey needs enough funding for a wallet change output",
      );
    outputs.splice(
      0,
      1,
      encodeMidgardTxOutput({
        ...output,
        address: protectMidgardAddress(output.address),
        value: {
          ...output.value,
          lovelace: output.value.lovelace - receiveLovelace,
        },
        script_ref: undefined,
      }),
      encodeMidgardTxOutput({
        ...scriptOutput,
        value: { lovelace: receiveLovelace, assets: new Map() },
      }),
    );
  }
  const transactionInput: FixtureTransactionInput = {
    spendInputs: [Buffer.from(spent[0], "hex")],
    outputs,
    fee: 0n,
    networkId: input.predecessor.header.expectedNetworkId,
    requiredSigners: [Buffer.from(key.to_public().hash().to_raw_bytes())],
    mintPolicyItems:
      variant === "nativeReceive"
        ? []
        : [
            encodeCbor([
              Buffer.from(policy, "hex"),
              new Map([[Buffer.alloc(0), 1n]]),
            ]),
          ],
    scriptWitnesses: [encodeMidgardVersionedScript(script)],
    redeemerWitnesses: variant === "plutusMint" ? [mintRedeemer] : [],
    scriptIntegrityHash:
      variant === "plutusMint"
        ? computeScriptIntegrityHashForLanguages(
            midgardFieldCommitment(encodeMidgardFieldPreimage([mintRedeemer])),
            ["PlutusV3"],
          )
        : undefined,
  };
  const unsigned = buildFixtureTransaction(transactionInput);
  const transaction = buildFixtureTransaction({
    ...transactionInput,
    addressWitnesses: [
      {
        verification_key: Buffer.from(key.to_public().to_raw_bytes()).toString(
          "hex",
        ),
        signature: key.sign(Buffer.from(unsigned.txId, "hex")).to_hex(),
      },
    ],
  });
  return {
    transaction,
    programMaterial:
      variant === "plutusMint" ? [...identityProgram.material.values()] : [],
  };
};

/** These claims accuse a valid transaction at a concrete forced-order coordinate. */
export const JOURNEY_SCRIPT_FORCED_REASONS = {
  nativeScriptDecoding: {
    ResolvedReferenceScriptMalformed: { source_kind: 0n, input_index: 0n },
  },
  nativeScriptInvalid: { WitnessNativeScriptFalse: { script_index: 0n } },
  witnessScriptDecoding: { WitnessNativeScriptMalformed: { script_index: 0n } },
  scriptIntegrityHashMissing: "ScriptIntegrityHashMissing",
  outputReferenceScriptDecoding: {
    OutputReferenceScriptMalformed: { output_index: 0n },
  },
  executionSourceScriptDecoding: {
    ExecutionNativeScriptMalformed: { execution_index: 0n },
  },
  receivePurposeLanguage: {
    ReceivePurposePlutusV3Forbidden: { execution_index: 0n },
  },
  unusedScriptWitness: { UnusedScriptWitness: { script_index: 0n } },
  missingScriptSource: {
    ScriptSourceMissing: { purpose_kind: 1n, purpose_index: 0n },
  },
  missingRedeemer: { RedeemerMissing: { purpose_kind: 1n, purpose_index: 0n } },
  unusedRedeemer: { UnusedRedeemer: { redeemer_index: 0n } },
  executionNativeScriptInvalid: {
    ExecutionNativeScriptFalse: { execution_index: 0n },
  },
  scriptIntegrityHashMismatch: "ScriptIntegrityHashMismatch",
  redeemerCanonicity: { RedeemerMalformed: { redeemer_index: 0n } },
} satisfies Partial<
  Record<SDK.FraudProofCatalogueCategoryName, SDK.RejectionReason>
>;

export type JourneyScriptForcedCategory =
  keyof typeof JOURNEY_SCRIPT_FORCED_REASONS;
export const JOURNEY_SCRIPT_FORCED_CATEGORIES = Object.keys(
  JOURNEY_SCRIPT_FORCED_REASONS,
) as JourneyScriptForcedCategory[];

export const prepareJourneyScriptForcedTransaction = (input: {
  category: JourneyScriptForcedCategory;
  predecessor: { header: SDK.Header; payload: SDK.DaPayload };
  ledgerOwnerSeedPhrase: string;
}) =>
  buildJourneyScriptTransaction({
    ...input,
    retainWalletChange: input.category === "receivePurposeLanguage",
    variant:
      input.category === "receivePurposeLanguage"
        ? "nativeReceive"
        : [
              "nativeScriptInvalid",
              "witnessScriptDecoding",
              "executionSourceScriptDecoding",
              "executionNativeScriptInvalid",
            ].includes(input.category)
          ? "nativeMint"
          : "plutusMint",
  });

export type JourneyScriptFixtureInput = {
  category: JourneyScriptForcedCategory;
  predecessor: {
    header: SDK.Header;
    headerHash: string;
    payload: SDK.DaPayload;
  };
  ledgerOwnerSeedPhrase: string;
  operatorVkey: string;
  endTime: bigint;
  blockSlot: bigint;
  /** Must be returned by the staged L1 forced-order action in live journeys. */
  orderKey: SDK.OutputReference;
};

export const buildJourneyScriptForcedFault = async (
  input: JourneyScriptFixtureInput,
) => {
  const material = prepareJourneyScriptForcedTransaction(input);
  const replayInput = {
    canonicalTransactionCbor: material.transaction.canonicalCbor,
    programMaterial: material.programMaterial,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.payload.block_body.utxos.map(
      ([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      }),
    ),
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
  };
  const control = await retainedTransactionFixture(replayInput);
  if (control.replay.trace.verdict !== "accepted")
    throw new Error(
      `Script journey ${input.category} valid control failed: ${control.replay.trace.rejectionCode}`,
    );
  const fault = await retainedTransactionFixture({
    ...replayInput,
    source: {
      kind: "forced",
      orderKey: input.orderKey,
      verdict: {
        ForcedTxInvalid: {
          reason: JOURNEY_SCRIPT_FORCED_REASONS[input.category],
        },
      },
    },
  });
  return { ...fault, control, material };
};

/** Replays the same on-chain order honestly after the malicious block is removed. */
export const buildJourneyScriptForcedSuccessor = async (
  input: JourneyScriptFixtureInput,
) => {
  const material = prepareJourneyScriptForcedTransaction(input);
  const block = await retainedTransactionFixture({
    canonicalTransactionCbor: material.transaction.canonicalCbor,
    programMaterial: material.programMaterial,
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.payload.block_body.utxos.map(
      ([key, value]) => ({
        outRef: Buffer.from(key, "hex"),
        output: Buffer.from(value, "hex"),
      }),
    ),
    operatorVkey: input.operatorVkey,
    endTime: input.endTime,
    blockSlot: input.blockSlot,
    source: {
      kind: "forced",
      orderKey: input.orderKey,
      verdict: "ForcedTxValid",
    },
  });
  if (block.replay.trace.verdict !== "accepted")
    throw new Error(
      `Honest forced successor failed: ${block.replay.trace.rejectionCode}`,
    );
  return block;
};
