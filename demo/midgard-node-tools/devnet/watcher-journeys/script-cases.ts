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
import { CML, walletFromSeed } from "@lucid-evolution/lucid";

import type { JourneyLedgerPredecessor } from "./journey-ledger.js";

/** `(lam ctx [(lam _ ()) ctx])`: a Plutus V3 script must return Unit. */
const unitScriptBytes = encodeCbor(Buffer.from("0101002324980041", "hex"));

/** An executable, signed transaction for real forced-order verdict disputes. */
export const buildJourneyScriptTransaction = (input: {
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
  variant?: "plutusMint" | "nativeMint";
  outputReferenceScript?: boolean;
}) => {
  const wallet = walletFromSeed(input.ledgerOwnerSeedPhrase, {
    network: "Custom",
  });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const owner = Buffer.from(key.to_public().hash().to_raw_bytes());
  const spent = input.predecessor.ledger.entries.find(({ output: bytes }) => {
    const credential = decodeMidgardAddressBytes(
      decodeMidgardTxOutput(bytes).address,
    ).paymentCredential;
    return (
      credential.kind === "PubKey" && Buffer.from(credential.hash).equals(owner)
    );
  });
  if (spent === undefined)
    throw new Error("Script journey needs a retained ledger input");
  const output = decodeMidgardTxOutput(spent.output);
  const variant = input.variant ?? "plutusMint";
  const script =
    variant === "plutusMint"
      ? {
          language: "PlutusV3" as const,
          scriptBytes: unitScriptBytes,
        }
      : decodeMidgardVersionedScript(Buffer.from("820043820180", "hex"));
  const policy = hashMidgardVersionedScript(script);
  const assets = new Map(
    [...output.value.assets].map(([hash, tokens]) => [hash, new Map(tokens)]),
  );
  const tokens = assets.get(policy) ?? new Map<string, bigint>();
  tokens.set("", (tokens.get("") ?? 0n) + 1n);
  assets.set(policy, tokens);
  const mintRedeemer = encodeMidgardRedeemerWitnessItem({
    purpose: "Mint",
    index: 0n,
    redeemerCbor: Buffer.from("00", "hex"),
    executionUnits: { memory: 5_000_000n, steps: 1_000_000_000n },
  });
  const scriptOutput = {
    ...output,
    address: protectMidgardAddress(output.address),
    value: { ...output.value, assets },
    script_ref:
      input.outputReferenceScript === false
        ? undefined
        : decodeMidgardVersionedScript(Buffer.from("820043820180", "hex")),
  };
  const outputs = [encodeMidgardTxOutput(scriptOutput)];
  const transactionInput: FixtureTransactionInput = {
    spendInputs: [spent.outRef],
    outputs,
    fee: 0n,
    networkId: input.predecessor.header.expectedNetworkId,
    requiredSigners: [Buffer.from(key.to_public().hash().to_raw_bytes())],
    mintPolicyItems: [
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
  redeemerCanonicity: { RedeemerMalformed: { redeemer_index: 0n } },
  missingRedeemer: {
    RedeemerMissing: { purpose_kind: 2n, purpose_index: 0n },
  },
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
  predecessor: JourneyLedgerPredecessor;
  ledgerOwnerSeedPhrase: string;
}) =>
  buildJourneyScriptTransaction({
    ...input,
    variant: ["nativeScriptInvalid", "witnessScriptDecoding"].includes(
      input.category,
    )
      ? "nativeMint"
      : "plutusMint",
  });

export type JourneyScriptFixtureInput = {
  category: JourneyScriptForcedCategory;
  predecessor: JourneyLedgerPredecessor;
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
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
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
    predecessor: input.predecessor,
    ledgerEntries: input.predecessor.ledger.entries,
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
