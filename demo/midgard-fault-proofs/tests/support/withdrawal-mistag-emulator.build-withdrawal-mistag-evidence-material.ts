import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../../src/transition-trace/phas.js";
import { type WithdrawalMistagCatalogueCategory } from "../../src/withdrawal-mistag/index.js";
import { makeFaultProofEmulatorHarness } from "./submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

export type WithdrawalMistagDirectionFixture =
  | "valid-marked-invalid"
  | "invalid-marked-valid";

export const buildWithdrawalMistagEvidenceMaterial = async (
  direction: WithdrawalMistagDirectionFixture,
  honest = false,
  outputBytes = 0,
  assetCount = 0,
  payoutDatumBytes = 0,
  proofLevels = 0,
  maximumAssetNames = false,
) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const withdrawalId: SDK.OutputReference = {
    transactionId:
      direction === "valid-marked-invalid" ? "41".repeat(32) : "42".repeat(32),
    outputIndex: 0n,
  };
  const lovelace =
    assetCount > 0
      ? 100_000_000n
      : direction === "valid-marked-invalid"
        ? 1_000_000n
        : 1n;
  const tokenEntries = Array.from({ length: assetCount }, (_, i) => {
    const name =
      assetCount === 1304
        ? i === 0
          ? ""
          : i <= 256
            ? (i - 1).toString(16).padStart(2, "0")
            : (i - 257).toString(16).padStart(4, "0")
        : maximumAssetNames
          ? i.toString(16).padStart(64, "0")
          : proofLevels > 0
            ? (255 - Math.floor(i / 32)).toString(16) + "00".repeat(i % 32)
            : i.toString(16).padStart(4, "0");
    return [name, assetCount === 1304 && i === 1303 ? 256n : 1n] as const;
  });
  const tokenAssets =
    assetCount === 0
      ? new Map<string, Map<string, bigint>>()
      : new Map([["aa".repeat(28), new Map(tokenEntries)]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: withdrawalId,
    l2_owner: owner,
    l2_value: new Map([["", new Map([["", lovelace]])], ...tokenAssets]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum:
      payoutDatumBytes === 0
        ? "NoDatum"
        : {
            InlineDatum: {
              data: Array.from(
                { length: Math.ceil(payoutDatumBytes / 64) },
                (_, i) => "ab".repeat(Math.min(64, payoutDatumBytes - i * 64)),
              ),
            },
          },
  };
  const message = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(SDK.withdrawalBodyBytes(body), "hex"),
    ]),
  );
  const info: SDK.WithdrawalInfo = {
    body,
    signature: [
      Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      privateKey.sign(message).to_hex(),
    ],
    validity:
      (direction === "valid-marked-invalid") !== honest
        ? "UnpayableWithdrawalValue"
        : "WithdrawalIsValid",
  };

  let padding = 0;
  const output = () =>
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
      value: { lovelace, assets: tokenAssets },
      ...(outputBytes === 0
        ? {}
        : {
            script_ref: {
              language: "PlutusV3" as const,
              scriptBytes: Buffer.alloc(padding, 1),
            },
          }),
    });
  let outputCbor = output();
  if (outputBytes !== 0) {
    for (let i = 0; i < 4 && outputCbor.length !== outputBytes; i++) {
      padding += outputBytes - outputCbor.length;
      outputCbor = output();
    }
    if (outputCbor.length !== outputBytes)
      throw new Error("withdrawal maximum output fixture length mismatch");
  }

  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor,
  });
  if (assetCount === 1304 && material.descriptor.cardanoValueSize !== 5000)
    throw new Error("withdrawal maximum Value fixture mismatch");
  const ledgerKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(withdrawalId.transactionId, "hex"),
    outputIndex: 0,
  });
  let ledger = await keyValuePhasRootWithCount([
    { key: ledgerKey, value: material.descriptorCbor },
  ]);
  let ledgerProof = await keyValuePhasProof(
    ledger,
    ledgerKey,
    material.descriptorCbor,
  );

  if (proofLevels > 0) {
    const deep = syntheticDeepMembershipProof({
      key: ledgerKey,
      value: material.descriptorCbor,
      branchLevels: proofLevels,
    });
    ledger = { ...ledger, root: deep.transactionsPhasRoot };
    ledgerProof = Data.from(deep.proofCbor, SDK.Proof);
  }
  const countedMembership = async (
    domain: SDK.RootDomain,
    key: Buffer,
    value: Buffer,
  ) => {
    const counted = await buildCountedRoot(domain, [{ key, value }]);
    if (proofLevels === 0)
      return {
        counted,
        proof: await keyValuePhasProof(
          { ...counted, root: counted.phasRoot },
          key,
          value,
        ),
      };
    const deep = syntheticDeepMembershipProof({
      key,
      value,
      branchLevels: proofLevels,
    });
    const phasRoot = deep.transactionsPhasRoot;
    return {
      counted: {
        ...counted,
        phasRoot,
        root: await Effect.runPromise(
          SDK.commitCountedRootProgram({
            domain,
            phasRoot,
            count: counted.count,
          }),
        ),
      },
      proof: Data.from(deep.proofCbor, SDK.Proof),
    };
  };

  const sourceKey = Buffer.from(
    SDK.committedWithdrawalKeyBytes(withdrawalId),
    "hex",
  );
  const sourceValue = Buffer.from(
    SDK.committedWithdrawalValueBytes(info),
    "hex",
  );
  const { counted: source, proof: sourceProof } = await countedMembership(
    SDK.ROOT_DOMAINS.withdrawals,
    sourceKey,
    sourceValue,
  );

  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: withdrawalId },
  };
  const eventValue: SDK.EventToStepValue = {
    step_index: 0n,
    phase: "Withdrawal",
  };
  const eventKeyBytes = Buffer.from(Data.to(eventKey, SDK.EventKey), "hex");
  const eventValueBytes = Buffer.from(
    Data.to(eventValue, SDK.EventToStepValue),
    "hex",
  );
  const { counted: event, proof: eventProof } = await countedMembership(
    SDK.ROOT_DOMAINS.eventToStep,
    eventKeyBytes,
    eventValueBytes,
  );

  const transitionValue: SDK.TransitionStep = {
    schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
    step_index: 0n,
    event_key: eventKey,
    phase: "Withdrawal",
    pre_utxos_root: ledger.root,
    post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const transitionKeyBytes = Buffer.from(Data.to(0n), "hex");
  const transitionValueBytes = Buffer.from(
    Data.to(transitionValue, SDK.TransitionStep),
    "hex",
  );
  const { counted: trace, proof: traceProof } = await countedMembership(
    SDK.ROOT_DOMAINS.transitionTrace,
    transitionKeyBytes,
    transitionValueBytes,
  );

  return {
    source,
    event,
    trace,
    ledger,
    args: {
      committedWithdrawal: {
        domain: SDK.ROOT_DOMAINS.withdrawals,
        root: source.root,
        phas_root: source.phasRoot,
        count: source.count,
        key: withdrawalId,
        value: info,
        proof: sourceProof,
      },
      eventToStep: {
        domain: SDK.ROOT_DOMAINS.eventToStep,
        root: event.root,
        phas_root: event.phasRoot,
        count: event.count,
        key: eventKey,
        value: eventValue,
        proof: eventProof,
      },
      transitionStep: {
        domain: SDK.ROOT_DOMAINS.transitionTrace,
        root: trace.root,
        phas_root: trace.phasRoot,
        count: trace.count,
        key: 0n,
        value: transitionValue,
        proof: traceProof,
      },
      ledgerEvidence: {
        PresentLedgerOutput: {
          output_cbor: outputCbor.toString("hex"),
          membership_proof: ledgerProof,
        },
      },
    },
  };
};

export const makeWithdrawalMistagEmulatorHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realWithdrawalMistag: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const withdrawalMistag = harness.contracts.withdrawalMistag;
  const rawCategory = harness.catalogue.categories.withdrawalMistag;
  if (withdrawalMistag === undefined || rawCategory === undefined) {
    throw new Error(
      "Harness did not build withdrawal-mistag contracts/category",
    );
  }
  if (rawCategory.categoryId !== SDK.WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID) {
    throw new Error("Unexpected withdrawal-mistag category id");
  }
  const category: WithdrawalMistagCatalogueCategory = {
    ...rawCategory,
    categoryId: SDK.WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID,
  };
  return { ...harness, withdrawalMistag, category };
};
