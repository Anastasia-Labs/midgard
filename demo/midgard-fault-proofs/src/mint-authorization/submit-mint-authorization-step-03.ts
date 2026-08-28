/**
 * `mint-authorization` step-03 submitters — the direction dispatch.
 *
 * Two arms, two entry points:
 *
 * - `submitMintAuthorizationStep03WitnessAbsence` — direction A's inline
 *   half. Opens the whole committed field 6 through the §8.8 door and lets
 *   the validator's fold prove no inline script witness (of any language)
 *   hashes to the claimed policy id, then routes into step-04's
 *   reference-input scan at cursor 0.
 * - `submitMintAuthorizationStep03EvaluateUnsatisfied` — direction B. Pins
 *   the policy's native payload by hash, opens the committed field 7 for
 *   the signer set, and lets the machine-twin evaluator refute the script
 *   against the committed signers and validity interval. Closes straight to
 *   step-05.
 *
 * Every check the validator makes that this process can make locally is
 * made locally first: the wrong-direction thread, an inline script that
 * DOES hash to the policy, a payload that does not hash-pin, a payload the
 * committed signer set actually satisfies — all refused before anything is
 * paid for.
 */
import {
  decodeMidgardAddressWitnessFieldPreimageV1,
  decodeMidgardNativeScript,
  decodeMidgardScriptWitnessFieldPreimageV1,
  hashMidgardVersionedScript,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationStep03Args,
  MintAuthorizationStep03StateV1,
  MintAuthorizationStep04StateV1,
  MintAuthorizationStep05StateV1,
  NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";
import {
  fieldOpeningV1ForField,
  hashHexWithBlake2b,
  MIDGARD_FIELD_INDEX_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1,
  MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
  MintAuthorizationStep03Datum,
  MintAuthorizationStep03SpendRedeemer,
  MintAuthorizationStep04Datum,
  MintAuthorizationStep05Datum,
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
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MintAuthorizationContractsV1 } from "./contracts-v1.js";
import {
  mintAuthorizationStepLabelV1,
  mintAuthorizationSubmitError,
  requireMintAuthorizationReferenceScriptV1,
  requireMintAuthorizationStepStateV1,
  requireMintAuthorizationThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = mintAuthorizationStepLabelV1(2);

export type SubmitMintAuthorizationStep03Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nextStepAddress: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step03Shared = {
  readonly lucid: LucidEvolution;
  readonly contracts: MintAuthorizationContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The bound transaction's compact CBOR, hex. */
  readonly nativeTxCompactCbor: string;
  /** The bound transaction's witness-set compact (three §5.1 hashes). */
  readonly witnessSet: NativeTxWitnessSetCompact;
  /** The published step-03 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
  readonly awaitConfirmation?: boolean;
};

const prepareThread = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
}: Step03Shared) => {
  const { threadUtxo, threadToken } =
    await requireMintAuthorizationThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 2,
      threadOutRef,
    });
  const state: MintAuthorizationStep03StateV1 =
    requireMintAuthorizationStepStateV1({
      threadUtxo,
      signer,
      schema: MintAuthorizationStep03Datum,
      stepIndex: 2,
    });
  return { threadUtxo, threadToken, state };
};

const submitPreparedStep03 = async ({
  shared,
  threadUtxo,
  threadToken,
  nextStepIndex,
  nextStepDatum,
  argsOf,
}: {
  readonly shared: Step03Shared;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly nextStepIndex: 3 | 4;
  readonly nextStepDatum: string;
  readonly argsOf: (layout: {
    readonly inputIndex: bigint;
    readonly outputIndex: bigint;
  }) => MintAuthorizationStep03Args;
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { lucid, contracts, signer, referenceScriptUtxo } = shared;
  const awaitConfirmation = shared.awaitConfirmation ?? true;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const nextOutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextStepDatum,
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
        nextOutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      { Continue: [argsOf(layout)] },
      MintAuthorizationStep03SpendRedeemer,
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
      contracts.steps[nextStepIndex].spendingScriptAddress,
      { kind: "inline", value: nextStepDatum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx =
    referenceScriptUtxo === undefined
      ? base.attach.SpendingValidator(contracts.steps[2].spendingScript)
      : base.readFrom([
          requireMintAuthorizationReferenceScriptV1({
            utxo: referenceScriptUtxo,
            expectedScriptHash: contracts.steps[2].spendingScriptHash,
            stepIndex: 2,
          }),
        ]);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw mintAuthorizationSubmitError(
      "BuildTxWithRedeemer did not resolve the step-03 layout.",
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
    threadOutRef: shared.threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nextStepAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

/**
 * Direction A's inline half: the committed field 6 holds no script — of any
 * language — hashing to the claimed policy id.
 */
export const submitMintAuthorizationStep03WitnessAbsence = async ({
  scriptTxWitsPreimageCborHex,
  ...shared
}: Step03Shared & {
  /** The committed field-6 preimage bytes, hex — the Inline carriage. */
  readonly scriptTxWitsPreimageCborHex: string;
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { threadUtxo, threadToken, state } = await prepareThread(shared);
  if (state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_ABSENT_V1) {
    throw mintAuthorizationSubmitError(
      `the thread's direction is ${state.direction.toString()}; the WitnessAbsence arm is direction A (0).`,
    );
  }
  // Doomed-transaction refusal: an inline script hashing to the policy
  // makes the absence fold fail on-chain.
  const inlineScripts = decodeMidgardScriptWitnessFieldPreimageV1(
    Buffer.from(scriptTxWitsPreimageCborHex, "hex"),
  );
  for (const script of inlineScripts) {
    if (hashMidgardVersionedScript(script) === state.policy_id) {
      throw mintAuthorizationSubmitError(
        `the committed field 6 carries a script hashing to the claimed policy ${state.policy_id} — the absence claim is false.`,
      );
    }
  }
  const opening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    carriage: { Inline: { preimage: scriptTxWitsPreimageCborHex } },
    witnessSet: shared.witnessSet,
  });
  const step04State: MintAuthorizationStep04StateV1 = {
    policy_id: state.policy_id,
    bad_tx_id: state.bad_tx_id,
    prior_ledger_root: state.prior_ledger_root,
    ref_cursor: 0n,
  };
  const step04Datum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step04State },
    MintAuthorizationStep04Datum,
  );
  return submitPreparedStep03({
    shared,
    threadUtxo,
    threadToken,
    nextStepIndex: 3,
    nextStepDatum: step04Datum,
    argsOf: (layout) => ({
      WitnessAbsence: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        script_tx_wits_opening: opening,
      },
    }),
  });
};

/**
 * Direction B: the policy's native payload, pinned by hash, evaluates
 * unsatisfied against the committed signer set and validity interval.
 */
export const submitMintAuthorizationStep03EvaluateUnsatisfied = async ({
  scriptBytesHex,
  addrTxWitsPreimageCborHex,
  ...shared
}: Step03Shared & {
  /** The policy's canonical native payload bytes, hex. */
  readonly scriptBytesHex: string;
  /** The committed field-7 preimage bytes, hex — the Inline carriage. */
  readonly addrTxWitsPreimageCborHex: string;
}): Promise<SubmitMintAuthorizationStep03Result> => {
  const { threadUtxo, threadToken, state } = await prepareThread(shared);
  if (state.direction !== MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1) {
    throw mintAuthorizationSubmitError(
      `the thread's direction is ${state.direction.toString()}; the EvaluateUnsatisfied arm is direction B (1).`,
    );
  }
  const decoded = decodeMidgardNativeScript(Buffer.from(scriptBytesHex, "hex"));
  const pinnedHash = hashMidgardVersionedScript({
    language: "NativeCardano",
    scriptBytes: decoded.cbor,
    nativeScript: decoded.script,
  });
  if (pinnedHash !== state.policy_id) {
    throw mintAuthorizationSubmitError(
      `the supplied native payload hashes to ${pinnedHash}, not the claimed policy ${state.policy_id}.`,
    );
  }
  const witnesses = decodeMidgardAddressWitnessFieldPreimageV1(
    Buffer.from(addrTxWitsPreimageCborHex, "hex"),
  );
  const signerHashes = new Set(
    witnesses.map((witness) =>
      Effect.runSync(
        hashHexWithBlake2b(
          Buffer.from(witness.verificationKey).toString("hex"),
          28,
        ),
      ),
    ),
  );
  const satisfied = verifyMidgardNativeScript(decoded.script, {
    validityIntervalStart:
      state.validity_interval_start === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : state.validity_interval_start,
    validityIntervalEnd:
      state.validity_interval_end === MIDGARD_POSIX_TIME_NONE
        ? undefined
        : state.validity_interval_end,
    witnessSigners: signerHashes,
  });
  if (satisfied) {
    throw mintAuthorizationSubmitError(
      "the committed signer set and validity interval SATISFY the policy's native script — there is no fault to prove.",
    );
  }
  const opening = fieldOpeningV1ForField({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
    nativeTxCompactCbor: shared.nativeTxCompactCbor,
    carriage: { Inline: { preimage: addrTxWitsPreimageCborHex } },
    witnessSet: shared.witnessSet,
  });
  const step05State: MintAuthorizationStep05StateV1 = {
    policy_id: state.policy_id,
    direction: MINT_AUTHORIZATION_DIRECTION_SCRIPT_UNSATISFIED_V1,
  };
  const step05Datum = Data.to(
    { fraud_prover: shared.signer.paymentKeyHash, data: step05State },
    MintAuthorizationStep05Datum,
  );
  return submitPreparedStep03({
    shared,
    threadUtxo,
    threadToken,
    nextStepIndex: 4,
    nextStepDatum: step05Datum,
    argsOf: (layout) => ({
      EvaluateUnsatisfied: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        script_bytes: scriptBytesHex,
        addr_tx_wits_opening: opening,
      },
    }),
  });
};
