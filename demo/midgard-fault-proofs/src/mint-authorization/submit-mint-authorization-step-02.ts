/**
 * `mint-authorization` step-02 submitter.
 *
 * Binds the thread to the committed claim and reads the accused policy id
 * off the committed field-5 item. The disputed header rides the redeemer,
 * the event's transition step and event→step leaf are opened from the
 * transition-trace reconstruction, and the committed mint field opens
 * through the §8.8 door on the Inline carriage. Every check the validator
 * makes that this process can make locally is made locally first, so a
 * doomed transaction is refused before it costs anything:
 *
 * - the reconstruction's header must hash to the thread NFT's asset-name
 *   tail (blake2b-224 of the serialised header Data);
 * - the accused ordinal must land inside the decoded committed mint field
 *   (the decode also re-asserts §5.6 canonicality — non-canonical committed
 *   bytes are the decoding family's dispute, not this one's);
 * - the direction must be in the family's two-value domain.
 *
 * The policy id in the step-03 state is READ off the committed item — the
 * caller names only the ordinal, exactly like the validator.
 */
import { decodeMidgardMintFieldPreimageV1 } from "@al-ft/midgard-core";
import type { MintAuthorizationStep03StateV1 } from "@al-ft/midgard-sdk";
import {
  fieldOpeningV1ForField,
  hashHexWithBlake2b,
  HeaderV1,
  MIDGARD_FIELD_INDEX_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
  MintAuthorizationStep02Datum,
  MintAuthorizationStep02SpendRedeemer,
  MintAuthorizationStep03Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MintAuthorizationContractsV1 } from "./contracts-v1.js";
import { buildMintAuthorizationStep02EvidenceV1 } from "./evidence-v1.js";
import {
  mintAuthorizationStepLabelV1,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = mintAuthorizationStepLabelV1(1);

export type SubmitMintAuthorizationStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The step-03 state the thread now carries. */
  readonly step03State: MintAuthorizationStep03StateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMintAuthorizationStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  reconstruction,
  policyIndex,
  direction,
  nativeTxCompactCbor,
  mintPreimageCborHex,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The disputed block's transition-trace reconstruction. */
  readonly reconstruction: TransitionTraceReconstruction;
  /** Ordinal of the accused field-5 policy item. */
  readonly policyIndex: bigint;
  /** 0 (script absent) or 1 (script unsatisfied). */
  readonly direction: bigint;
  /** The bound transaction's compact CBOR, hex (the id preimage). */
  readonly nativeTxCompactCbor: string;
  /** The committed field-5 preimage bytes, hex — the Inline carriage. */
  readonly mintPreimageCborHex: string;
  /** The published step-02 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMintAuthorizationStep02Result> => {
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    });
  const anchorState = requireMintAuthorizationStepStateV1({
    threadUtxo,
    signer,
    schema: MintAuthorizationStep02Datum,
    stepIndex: 1,
  });

  // The header must be the thread NFT's: category id ‖ blake2b-224(header).
  const headerHash = await Effect.runPromise(
    hashHexWithBlake2b(Data.to(reconstruction.header, HeaderV1), 28),
  );
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw mintAuthorizationSubmitError(
      `the reconstruction's header hashes to ${headerHash}, not the thread NFT's disputed header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  if (
    direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1 &&
    direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1
  ) {
    throw mintAuthorizationSubmitError(
      `direction ${direction.toString()} is outside {0, 1}.`,
    );
  }
  const mintItems = decodeMidgardMintFieldPreimageV1(
    Buffer.from(mintPreimageCborHex, "hex"),
  );
  if (policyIndex < 0n || policyIndex >= BigInt(mintItems.length)) {
    throw mintAuthorizationSubmitError(
      `policy index ${policyIndex.toString()} is outside the committed mint field's ${mintItems.length.toString()} items.`,
    );
  }
  const accusedPolicyId = Buffer.from(
    mintItems[Number(policyIndex)].policyId,
  ).toString("hex");

  const evidence = await buildMintAuthorizationStep02EvidenceV1({
    reconstruction,
    eventKey: { L2TransactionEventKey: { tx_id: anchorState.bad_tx_id } },
  });
  const priorLedgerRoot =
    evidence.transitionStepMembership.value.pre_utxos_root;

  const step03State: MintAuthorizationStep03StateV1 = {
    policy_id: accusedPolicyId,
    direction,
    bad_tx_id: anchorState.bad_tx_id,
    bad_tx_witness_set_hash: anchorState.bad_tx_witness_set_hash,
    validity_interval_start: anchorState.validity_interval_start,
    validity_interval_end: anchorState.validity_interval_end,
    prior_ledger_root: priorLedgerRoot,
  };

  const mintOpening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.mint,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: mintPreimageCborHex } },
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step03State },
    MintAuthorizationStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            header: reconstruction.header,
            event_to_step_membership: evidence.eventToStepMembership,
            transition_step_membership: evidence.transitionStepMembership,
            policy_index: policyIndex,
            direction,
            mint_opening: mintOpening,
          },
        ],
      },
      MintAuthorizationStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      contracts.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[1].spendingScript)
      : base.readFrom([
          requireMintAuthorizationReferenceScriptV1({
            utxo: referenceScriptUtxo,
            expectedScriptHash: contracts.steps[1].spendingScriptHash,
            stepIndex: 1,
          }),
        ]);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    thirdStepAddress: contracts.steps[2].spendingScriptAddress,
    step03State,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
