import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  buildValidationTraceDisputeFaultProofContracts,
  parseFaultProofBlueprint,
  type ValidationTraceDisputeFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  CML,
  credentialToAddress,
  type LucidEvolution,
  type MintingPolicy,
  mintingPolicyToId,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
} from "../src/index.js";
import {
  fundingLovelaceForOutputs,
  makeMinAdaFundedExactSizeOutputItem,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

/**
 * **The tiers-2/3 leg of the canonical-decode staged chain, against the applied
 * validators** — `docs/spec/midgard-tx.md` §8, issues #600 and #579.
 *
 * #600 landed the producer and cross-language-vector halves of the §8.4
 * carriage seam and stopped there on the orchestrator's Ruling 2: a tiers-2/3
 * emulator row built against the then-frozen blueprint would have failed with
 * the same `Spend[1] unexpected empty list` signature as five unrelated rows,
 * whatever its own correctness, and an unfalsifiable red proves nothing. #579's
 * regeneration removed that obstacle. This file is the row that was owed.
 *
 * **What is actually exercised.** `canonical_decode_item_observe_v1` is the one
 * stage of the complete-item chain that *dereferences* the carriage — the
 * semantic and source stages only hash it into `evidence_hash` — so it is the
 * only place a tier-2 or tier-3 index means anything, and it is the only stage
 * carrying the §8.6 `field_preimage_certificate_policy_id` parameter. Each row
 * below therefore drives the whole chain (`Verify` → source → `Observe` → proof
 * → settlement) over a field-2 preimage §8.4 places above the 14,336-byte
 * tier-1 cap, with the carriage published to, and resolved back out of, a real
 * emulator ledger.
 *
 * **Nothing here is hand-written.** The preimage comes from the machine's own
 * trace, the tier from `selectMidgardFieldCarriageTier`, the plan from
 * `planMidgardFieldCarriage`, the publications and the §8.6 certification
 * from the SDK builders, the indices from
 * `resolveMidgardFieldCarriageAgainstReferenceInputs` resolving by content
 * against the door transaction's own reference-input set, the committed
 * evidence from `buildValidationOneStepArgument`, and the staged datums from
 * `deriveCanonicalDecodeItemStageData` — the same function the production
 * submitter uses. There is no CBOR literal and no copied hash in this file.
 *
 * **The certificate is the compiled one.** Tier 3's door checks the named
 * reference input for one unit of the policy the observe validator was
 * *parameterised* with, so a stand-in policy cannot reach it: the certificate is
 * minted by `field_preimage_certificate.field_preimage_certificate.mint` out of
 * the same blueprint, and the parameterisation is pinned by rebuilding the
 * observe validator from that policy id and comparing script hashes.
 */

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

const blueprintJson = JSON.parse(readFileSync(blueprintPath, "utf8")) as {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

export const compiledScript = (title: string): string => {
  const validator = blueprintJson.validators.find(
    (candidate) => candidate.title === title,
  );
  if (validator === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return applyDoubleCborEncoding(validator.compiledCode);
};

export const NETWORK = "Custom" as const;

const HUB_ORACLE_POLICY_ID = "11".repeat(28);

const FRAUD_PROOF_CATALOGUE_POLICY_ID = "22".repeat(28);

export const THREAD_ASSET_NAME = "aa".repeat(32);

export const SIGNING_KEY = CML.PrivateKey.from_normal_bytes(
  Buffer.alloc(32, 7),
);

export const SIGNER_HASH = Buffer.from(
  SIGNING_KEY.to_public().hash().to_raw_bytes(),
).toString("hex");

export const MAX_L1_TX_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;

/** §2.5 field 2 — the outputs field, the one these traces read complete items of. */
export const OUTPUT_FIELD_INDEX = 2;

/**
 * `NoAuxiliaryWitness` as the committed evidence names it — the Option B
 * (#620) auxiliary half of the canonical-decode resolver's `evidence_hash`.
 * Same literal as `complete-item-route-adversarial-emulator.test.ts`.
 */
export const NO_AUXILIARY_WITNESS_CBOR = Buffer.from("d87980", "hex");

// ## The disputed transaction, and the step that reads its field 2

/**
 * RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1; R8 of decision 0005). This file
 * used to carry its own copy of the exact-size item builder, producing
 * 10-lovelace items that the ValueAndMint output-descriptor scan now convicts
 * with `E_MIN_ADA`. The shared builder funds each item at its own minimum-Ada
 * floor without moving its length, so every carriage measurement below
 * measures the same number of bytes it did before the wiring.
 */
export const makeExactSizeOutputItem = makeMinAdaFundedExactSizeOutputItem;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

/**
 * The two outputs whose §5.1 field-2 envelope is **exactly** `targetBytes`.
 *
 * Two rather than one, and that is the whole reason this helper exists. §8.4
 * partitions on the *field*'s preimage while the trace producer routes on the
 * *item*: an output past
 * `MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes` is decoded
 * by canonicalDecode's chunked route and emits no complete-item witness at all,
 * so a single-output field cannot reach tier 3 through this chain — its
 * complete-item step stops existing several thousand bytes below `chunk_bytes_k`.
 * Splitting the same byte count across two admissible items keeps every item on
 * the complete-item route while the field they belong to climbs the ladder,
 * which is the ordinary shape of a large transaction anyway.
 *
 * The envelope is `82 ‖ 59 LLLL ‖ item0 ‖ 59 LLLL ‖ item1` — seven bytes of
 * framing — and the result is checked against `encodeMidgardFieldPreimage`
 * rather than trusted.
 */
export const outputsForFieldTwoPreimageBytes = (
  targetBytes: number,
): readonly Buffer[] => {
  const payload = targetBytes - 7;
  const first = Math.floor(payload / 2);
  const outputs = [
    makeExactSizeOutputItem(first),
    makeExactSizeOutputItem(payload - first),
  ];
  const measured = encodeMidgardFieldPreimage(outputs).length;
  if (measured !== targetBytes) {
    throw new Error(
      `two-output field-2 envelope measured ${measured.toString()} bytes, wanted ${targetBytes.toString()}`,
    );
  }
  for (const output of outputs) {
    if (
      output.length >
      MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes
    ) {
      throw new Error(
        "output item is past the producer's complete-item threshold and would be chunked",
      );
    }
  }
  return outputs;
};

export const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  // Every generated item pays 10 lovelace, so the one input has to fund all of
  // them or the machine rejects with `E_VALUE_NOT_PRESERVED` before it ever
  // reaches canonicalDecode.
  // The resolved input has to fund every produced output now that each is
  // funded at its own minimum-Ada floor, or stage five would convict this
  // trace with `E_VALUE_NOT_PRESERVED` instead of accepting it. The fee is
  // zero, so the sum is exact.
  const spentOutput = makeOutput(fundingLovelaceForOutputs(outputs));
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [...outputs],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOp({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

/**
 * The `canonicalDecode` complete-item step for **field 2**, located by the bytes
 * it read.
 *
 * Selecting on `(phase, kind)` alone is not enough and the difference is not
 * cosmetic: the canonicalDecode walk emits a complete-item witness for field 0
 * first, whose preimage is a few dozen bytes, so a first-match selector silently
 * measures the wrong field — every tier assertion below would then be about a
 * tier-1 preimage. Matching on the preimage length is the same discipline
 * `complete-item-proof-fit.test.ts` keeps.
 */
export const findFieldTwoCompleteItemStep = (
  trace: DeterministicValidationMachineTrace,
  expectedPreimageBytes: number,
): { readonly stateIndex: number; readonly fieldPreimage: Buffer } => {
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase !== "canonicalDecode" ||
      witness.auxiliary?.kind !== "transactionFieldItem" ||
      witness.auxiliary.fieldIndex !== OUTPUT_FIELD_INDEX ||
      witness.auxiliary.fieldPreimage.length !== expectedPreimageBytes
    ) {
      continue;
    }
    return {
      stateIndex: index,
      fieldPreimage: witness.auxiliary.fieldPreimage,
    };
  }
  throw new Error(
    `trace has no canonicalDecode field-2 complete-item witness of ${expectedPreimageBytes.toString()} preimage bytes`,
  );
};

// ## The deployed contracts, and the §8.6 policy the door is parameterised by

export const OBSERVE_TITLE =
  "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main.spend";

export const CERTIFICATE_MINT_TITLE =
  "field_preimage_certificate.field_preimage_certificate.mint";

let cachedContracts: ValidationTraceDisputeFaultProofContracts | undefined;

export const loadContracts =
  async (): Promise<ValidationTraceDisputeFaultProofContracts> => {
    cachedContracts ??= await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parseFaultProofBlueprint(
          JSON.parse(JSON.stringify(blueprintJson)),
        ),
        network: NETWORK,
        hubOraclePolicyId: HUB_ORACLE_POLICY_ID,
        fraudProofCataloguePolicyId: FRAUD_PROOF_CATALOGUE_POLICY_ID,
        referenceScriptAuthPolicyId: "33".repeat(28),
      }),
    );
    return cachedContracts;
  };

/**
 * The compiled §8.6 certificate policy — mint and spend are handlers of one
 * validator, so the policy id *is* the address's payment credential, and the
 * mint handler refuses any output carrying a stake credential.
 */
export const certificatePolicy = (): {
  readonly script: MintingPolicy;
  readonly policyId: string;
  readonly address: string;
} => {
  const script: MintingPolicy = {
    type: "PlutusV3",
    script: compiledScript(CERTIFICATE_MINT_TITLE),
  };
  const policyId = mintingPolicyToId(script);
  return {
    script,
    policyId,
    address: credentialToAddress(NETWORK, scriptHashToCredential(policyId)),
  };
};

// ## Emulator harness

export type Harness = {
  readonly lucid: LucidEvolution;
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly threadUnit: string;
  readonly walletAddress: string;
};

export const WALLET_ADDRESS = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_pub_key(SIGNING_KEY.to_public().hash()),
)
  .to_address()
  .to_bech32();
