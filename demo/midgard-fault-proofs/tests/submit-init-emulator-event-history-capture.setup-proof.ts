import { readFileSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { setupHistoryPair } from "./support/emulator/history-pair.js";

export const blueprintBytes = readFileSync(realBlueprintPath);

export const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);

export const records: unknown[] = [];

export const protectionDurationMs = 120_000n;

export const timing = {
  visibilityBudgetMs: 20_000n,
  submissionBudgetMs: 40_000n,
  remainingProofBudgetMs: 80_000n,
  slotLengthMs: 1000n,
};

export type Harness = Awaited<ReturnType<typeof setupHistoryPair>>;

type Kind = "Deposit" | "Withdrawal";

export const familyIndex = (kind: Kind) => (kind === "Deposit" ? 0 : 1);

export const deployment = (h: Harness, kind: Kind) => {
  const a = h.applied[familyIndex(kind)]!;
  return {
    policyId: a.policyId,
    address: a.address,
    retentionAddress: a.retention.address,
    inlineLimitBytes: 512n,
  };
};

export const fetchWitness = (h: Harness, kind: Kind, id: SDK.OutputReference) =>
  SDK.fetchEventHistoryWitness(
    { utxosAt: (address) => h.lucid.utxosAt(address) },
    deployment(h, kind),
    id,
  );

// Divide precisely the gap containing the selected identity, or change an
// Order's own successor. Both cases consume the challenger's exact reference.
export const nextChurnKey = async (
  w: SDK.EventHistoryWitness,
  id: SDK.OutputReference,
) => {
  const target = BigInt(
    "0x" + (await Effect.runPromise(SDK.eventHistoryKey(id))),
  );
  const lower = BigInt("0x" + (w.anchor.key ?? "00".repeat(32)));
  const upper =
    w.kind === "Present"
      ? BigInt("0x" + (w.anchor.node.next ?? "ff".repeat(32)))
      : target;
  const key = (lower + upper) / 2n;
  if (key <= lower || key >= upper)
    throw new Error("Fixture exhausted its insertion gap");
  return key.toString(16).padStart(64, "0");
};

export const waitForMutation = (h: Harness, w: SDK.EventHistoryWitness) => {
  const remaining = w.anchor.node.protected_until - BigInt(h.emulator.now());
  if (remaining > 0n) h.emulator.awaitSlot(Number((remaining + 999n) / 1000n));
};

export const setupProof = async (
  h: Harness,
  kind: Kind,
  id: SDK.OutputReference,
  headerEnd: bigint,
  committedHash = "00".repeat(32),
) => {
  const title =
    kind === "Deposit" ? "fabricated_deposit" : "fabricated_withdrawal";
  const step03: Script = {
    type: "PlutusV3",
    script: SDK.applyBlueprintParams(
      blueprint,
      `fraud_proofs/${title}/step_03.main.spend`,
      [h.hubPolicy, h.hubPolicy],
    ),
  };
  const step02: Script = {
    type: "PlutusV3",
    script: SDK.applyBlueprintParams(
      blueprint,
      `fraud_proofs/${title}/step_02.main.spend`,
      [
        validatorToScriptHash(step03),
        h.hubPolicy,
        h.hubPolicy,
        Data.from(
          Data.to(
            Effect.runSync(
              SDK.addressDataFromBech32(deployment(h, kind).retentionAddress),
            ),
            SDK.AddressData,
          ),
        ),
        512n,
        5000n,
        512n,
      ],
    ),
  };
  const scripts = [step02, step03];
  const refs: UTxO[] = [];
  for (const script of scripts) {
    const hash = await h.submit(
      `publish-${kind}-capture-script`,
      await h.lucid
        .newTx()
        .collectFrom(await h.funding())
        .pay.ToAddressWithData(
          h.hubAddress,
          undefined,
          { lovelace: 80_000_000n },
          script,
        )
        .complete({ coinSelection: false, localUPLCEval: true }),
    );
    refs.push(
      (await h.lucid.utxosByOutRef([{ txHash: hash, outputIndex: 0 }]))[0]!,
    );
  }
  const common = {
    state_queue_policy: h.hubPolicy,
    challenged_header_hash: "ab".repeat(28),
    header_start_time: headerEnd - 100_000n,
    header_end_time: headerEnd,
  };
  const state02 =
    kind === "Deposit"
      ? Data.to(
          {
            fraud_prover: h.owner,
            data: {
              ...common,
              committed_deposit_id: id,
              committed_deposit_info_hash: committedHash,
            },
          },
          SDK.FabricatedDepositStep02Datum,
        )
      : Data.to(
          {
            fraud_prover: h.owner,
            data: {
              ...common,
              committed_withdrawal_id: id,
              committed_withdrawal_content_hash: committedHash,
            },
          },
          SDK.FabricatedWithdrawalStep02Datum,
        );
  const unit =
    h.hubPolicy +
    (kind === "Deposit"
      ? SDK.FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID
      : SDK.FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID) +
    common.challenged_header_hash;
  const txHash = await h.submit(
    `fixture-issue-${kind}-step02`,
    await h.lucid
      .newTx()
      .collectFrom(await h.funding())
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(h.issuer)
      .pay.ToContract(
        validatorToAddress("Custom", step02),
        { kind: "inline", value: state02 },
        { lovelace: 4_000_000n, [unit]: 1n },
      )
      .pay.ToAddress(h.wallet.address, { lovelace: 20_000_000n })
      .complete({ coinSelection: false, localUPLCEval: true }),
  );
  const [thread, fee] = await h.lucid.utxosByOutRef([
    { txHash, outputIndex: 0 },
    { txHash, outputIndex: 1 },
  ]);
  return {
    kind,
    id,
    headerEnd,
    common,
    committedHash,
    scripts,
    refs,
    thread: thread!,
    fee: fee!,
    unit,
  };
};

export type Proof = Awaited<ReturnType<typeof setupProof>>;
