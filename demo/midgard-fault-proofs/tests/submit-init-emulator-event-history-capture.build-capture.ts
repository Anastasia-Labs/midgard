import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type RedeemerContext,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  deployment,
  type Harness,
  type Proof,
} from "./submit-init-emulator-event-history-capture.setup-proof.js";

export const buildCapture = async (
  h: Harness,
  p: Proof,
  w: SDK.EventHistoryWitness,
  window: SDK.EventHistoryCaptureWindow,
) => {
  const refs = [
    h.hub,
    p.refs[0]!,
    w.anchor.utxo,
    ...(w.kind === "Present" && w.retainedDataUtxo ? [w.retainedDataUtxo] : []),
  ];
  const captured =
    w.kind === "Present"
      ? SDK.captureEventHistoryWitness(
          w,
          deployment(h, p.kind).policyId,
          p.kind,
        )
      : undefined;
  const nextStep = validatorToAddress("Custom", p.scripts[1]!);
  let datum: string;
  let redeemer: BuildTxWithRedeemer;
  const commonArgs = (ctx: RedeemerContext) => ({
    input_index: SDK.requireInputIndex(ctx, p.thread, "capture thread"),
    output_index: SDK.requireUniqueOutputIndex(
      ctx.outputs,
      (output) => output.address === nextStep,
      "capture step",
    ),
  });
  const hubIndex = (ctx: RedeemerContext) =>
    SDK.requireReferenceInputIndex(ctx, h.hub, "history hub");
  const anchorIndex = (ctx: RedeemerContext) =>
    SDK.requireReferenceInputIndex(ctx, w.anchor.utxo, "history anchor");
  const evidenceIndices = (ctx: RedeemerContext) => ({
    hub_ref_input_index: hubIndex(ctx),
    event_ref_input_index: anchorIndex(ctx),
    external_ref_input_index:
      w.kind === "Present" && w.retainedDataUtxo
        ? SDK.requireReferenceInputIndex(
            ctx,
            w.retainedDataUtxo,
            "history publication",
          )
        : null,
  });
  if (p.kind === "Deposit") {
    const state = Data.from(
      p.thread.datum!,
      SDK.FabricatedDepositStep02Datum,
    ).data!;
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedDepositStep03State(
          state,
          captured
            ? { DepositEventObserved: { commitment: captured.commitment } }
            : "DepositIdentityAbsent",
        ),
      },
      SDK.FabricatedDepositStep03Datum,
    );
    redeemer = (ctx) =>
      Data.to(
        {
          Continue: [
            {
              ...commonArgs(ctx),
              evidence: captured
                ? { PresentDepositEvent: evidenceIndices(ctx) }
                : {
                    AbsentDepositIdentity: {
                      hub_ref_input_index: hubIndex(ctx),
                      history_ref_input_index: anchorIndex(ctx),
                    },
                  },
            },
          ],
        },
        SDK.FabricatedDepositStep02SpendRedeemer,
      );
  } else {
    const state = Data.from(
      p.thread.datum!,
      SDK.FabricatedWithdrawalStep02Datum,
    ).data!;
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedWithdrawalStep03State(
          state,
          captured
            ? { WithdrawalEventObserved: { commitment: captured.commitment } }
            : "WithdrawalIdentityAbsent",
        ),
      },
      SDK.FabricatedWithdrawalStep03Datum,
    );
    redeemer = (ctx) =>
      Data.to(
        {
          Continue: [
            {
              ...commonArgs(ctx),
              evidence: captured
                ? { PresentWithdrawalEvent: evidenceIndices(ctx) }
                : {
                    AbsentWithdrawalIdentity: {
                      hub_ref_input_index: hubIndex(ctx),
                      history_ref_input_index: anchorIndex(ctx),
                    },
                  },
            },
          ],
        },
        SDK.FabricatedWithdrawalStep02SpendRedeemer,
      );
  }
  const tx = await h.lucid
    .newTx()
    .collectFrom([p.fee])
    .collectFrom([p.thread], redeemer)
    .readFrom(refs)
    .addSignerKey(h.owner)
    .validFrom(Number(window.validFrom))
    .validTo(Number(window.validTo))
    .pay.ToContract(nextStep, { kind: "inline", value: datum }, p.thread.assets)
    .complete({ coinSelection: false, localUPLCEval: true });
  return { tx, captured, datum };
};

export const continueCaptured = async (
  h: Harness,
  p: Proof,
  thread: UTxO,
  captured: ReturnType<typeof SDK.captureEventHistoryWitness> | undefined,
  badOpening = false,
) => {
  const funding = await h.funding();
  const commonArgs = (ctx: RedeemerContext) => ({
    input_index: SDK.requireInputIndex(ctx, thread, "capture thread"),
    output_index: SDK.requireUniqueOutputIndex(
      ctx.outputs,
      (output) => output.address === h.hubAddress,
      "capture step",
    ),
  });
  // Serialize and reopen preimage bytes without retrieving a historical L1 node.
  // Full process restart/persistence acceptance remains a separate gate.
  const opening = captured
    ? {
        RetainedEventData: {
          payload: captured.payload,
          original_assets: badOpening
            ? new Map([["", new Map([["", 1n]])]])
            : captured.originalAssets,
        },
      }
    : "NoAuthenticContent";
  let datum: string;
  let redeemer: BuildTxWithRedeemer;
  if (p.kind === "Deposit") {
    const state = Data.from(
      thread.datum!,
      SDK.FabricatedDepositStep03Datum,
    ).data!;
    const fault: SDK.FabricatedDepositFault = captured
      ? {
          IneligibleDepositEvent: {
            event_inclusion_time: captured.commitment.inclusion_time,
          },
        }
      : "NonexistentDepositIdentity";
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedDepositStep04State(state, fault),
      },
      SDK.FabricatedDepositStep04Datum,
    );
    const bytes = Data.to(
      opening,
      SDK.FabricatedDepositAuthenticContentOpening,
    );
    redeemer = (ctx) =>
      Data.to(
        {
          Continue: [
            {
              ...commonArgs(ctx),
              authentic_content: Data.from(
                bytes,
                SDK.FabricatedDepositAuthenticContentOpening,
              ),
            },
          ],
        },
        SDK.FabricatedDepositStep03SpendRedeemer,
      );
  } else {
    const state = Data.from(
      thread.datum!,
      SDK.FabricatedWithdrawalStep03Datum,
    ).data!;
    const fault: SDK.FabricatedWithdrawalFault = captured
      ? {
          IneligibleWithdrawalEvent: {
            event_inclusion_time: captured.commitment.inclusion_time,
          },
        }
      : "NonexistentWithdrawalIdentity";
    datum = Data.to(
      {
        fraud_prover: h.owner,
        data: SDK.fabricatedWithdrawalStep04State(state, fault),
      },
      SDK.FabricatedWithdrawalStep04Datum,
    );
    const bytes = Data.to(
      opening,
      SDK.FabricatedWithdrawalAuthenticContentOpening,
    );
    redeemer = (ctx) =>
      Data.to(
        {
          Continue: [
            {
              ...commonArgs(ctx),
              authentic_content: Data.from(
                bytes,
                SDK.FabricatedWithdrawalAuthenticContentOpening,
              ),
            },
          ],
        },
        SDK.FabricatedWithdrawalStep03SpendRedeemer,
      );
  }
  return h.lucid
    .newTx()
    .collectFrom(funding)
    .collectFrom([thread], redeemer)
    .readFrom([p.refs[1]!])
    .addSignerKey(h.owner)
    .validFrom(Math.max(Number(p.headerEnd) + 1000, h.emulator.now() - 60_000))
    .validTo(h.emulator.now() + 40_000)
    .pay.ToContract(
      h.hubAddress,
      { kind: "inline", value: datum },
      thread.assets,
    )
    .complete({ coinSelection: false, localUPLCEval: true });
};

export const productionContracts = (h: Harness, p: Proof) => {
  const step = (script: Script) => ({
    spendingScript: script,
    spendingScriptHash: validatorToScriptHash(script),
    spendingScriptAddress: validatorToAddress("Custom", script),
  });
  return {
    history: {
      inlineLimitBytes: 512n,
      maxPayloadBytes: 5000n,
      maxPayloadNodes: 512n,
      retentionAddress: deployment(h, p.kind).retentionAddress,
    },
    steps: [
      step(p.scripts[0]!),
      step(p.scripts[0]!),
      step(p.scripts[1]!),
      step(h.issuer),
    ] as const,
    computationThread: { policyId: h.hubPolicy, mintingScript: h.issuer },
    fraudProof: {
      policyId: h.hubPolicy,
      mintingScript: h.issuer,
      spendingScriptAddress: h.hubAddress,
    },
    hubOraclePolicyId: h.hubPolicy,
    stateQueuePolicyId: h.hubPolicy,
    categoryId:
      p.kind === "Deposit"
        ? SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID
        : SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  };
};
