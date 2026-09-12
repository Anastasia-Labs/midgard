import { existsSync } from "node:fs";
import { join } from "node:path";

import {
  decodeMidgardTxOutput,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  type TxSignBuilder,
  type UTxO,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { publishWorkflowDeploymentOnChain } from "midgard-node/tests/helpers/published-workflow-deployment";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import {
  ownedHistoryLedgerEntries,
  retainHistoryTransactions,
  signHistoryTransaction,
} from "./history-cases.js";
import {
  buildJourneyFabricatedDeposit,
  buildJourneyWithdrawalEvent,
  buildJourneyWithdrawnTransaction,
  journeyWithdrawalBody,
  type StagedHistoryEvent,
} from "./history-events.js";
import {
  createPreparedJourneyFixture,
  decodeJourneyRetainedBlock,
  type JourneyFaultPreparationInput,
} from "./staging.js";

export type EventMetadata = {
  address: string;
  unit: string;
  inclusionTime: number;
  validTo: number;
};
export type HistoryEventCheckpoint = {
  deploymentFingerprint: string;
  request: string;
  signedCbor: string;
  txHash: string;
  metadata: EventMetadata;
  outcome: "prepared" | "submitted" | "confirmed" | "unknown";
  event?: UTxO;
};

/**
 * Submit one recorded SDK-built event. Restart reconciles its canonical output
 * and resubmits only the original bytes. Unknown outcomes never allocate a new
 * nonce. The common runner serializes this with all other wallet spending.
 */
const publishEvent = async (
  input: JourneyFaultPreparationInput,
  request: JourneyEventPublicationRequest,
): Promise<StagedHistoryEvent> => {
  const { context, directory, onStage } = input;
  const { deployment, provider } = context;
  const lucid = deployment.operatorLucid;
  const path = join(directory, `${request.name}.json`);
  let checkpoint: HistoryEventCheckpoint;
  if (existsSync(path)) {
    checkpoint = await readJourneyArtifact<HistoryEventCheckpoint>(path);
    if (
      checkpoint.deploymentFingerprint !== deployment.manifest.manifestId ||
      checkpoint.request !== request.identity
    )
      throw new Error(
        "Event checkpoint differs from the deployment or exact requested action",
      );
    const hash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(checkpoint.signedCbor).body(),
    ).to_hex();
    if (hash !== checkpoint.txHash)
      throw new Error(
        "Event checkpoint signed bytes differ from recorded hash",
      );
  } else {
    const funding = (
      await lucid.utxosAt(await lucid.wallet().address())
    ).filter(
      (utxo) =>
        utxo.datum == null &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
    );
    if (funding.length === 0)
      throw new Error("No ordinary funding inputs for event publication");
    lucid.overrideUTxOs(funding);
    const built = await request.build();
    const signed = await built.tx.sign.withWallet().complete();
    checkpoint = {
      deploymentFingerprint: deployment.manifest.manifestId,
      request: request.identity,
      signedCbor: signed.toCBOR(),
      txHash: signed.toHash(),
      metadata: built.metadata,
      outcome: "prepared",
    };
    await writeJourneyArtifact(path, checkpoint);
  }
  const eventOutputs = () =>
    provider.getUtxosWithUnit(
      checkpoint.metadata.address,
      checkpoint.metadata.unit,
    );
  let outputs = await eventOutputs();
  if (outputs.length > 1) throw new Error("Event role token is duplicated");
  if (outputs.length === 0) {
    if (checkpoint.outcome === "confirmed")
      throw new Error(
        "Confirmed event is unavailable; reconcile protocol consumption before resuming",
      );
    if (deployment.chain.now() >= checkpoint.metadata.validTo)
      throw new Error(
        `Recorded event ${checkpoint.txHash} expired without an available output; explicit reconciliation is required`,
      );
    onStage(`${request.name} publication`);
    checkpoint.outcome = "unknown";
    await writeJourneyArtifact(path, checkpoint);
    try {
      const hash = await provider.submitTx(checkpoint.signedCbor);
      if (hash !== checkpoint.txHash)
        throw new Error("Provider changed signed event transaction hash");
      checkpoint.outcome = "submitted";
      await writeJourneyArtifact(path, checkpoint);
    } catch (cause) {
      outputs = await eventOutputs();
      if (outputs.length !== 1)
        throw new Error(
          `Event ${checkpoint.txHash} submission needs reconciliation; exact signed record retained`,
          { cause },
        );
    }
    await provider.awaitTx(checkpoint.txHash, 500);
    outputs = await eventOutputs();
  }
  const event = outputs[0];
  if (
    outputs.length !== 1 ||
    event === undefined ||
    event.txHash !== checkpoint.txHash
  )
    throw new Error("Confirmed event differs from the recorded transaction");
  checkpoint.outcome = "confirmed";
  checkpoint.event = event;
  await writeJourneyArtifact(path, checkpoint);
  lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
  onStage(`${request.name} inclusion interval`);
  await deployment.chain.awaitSlot(
    Math.max(
      0,
      Math.ceil(
        (checkpoint.metadata.inclusionTime - deployment.chain.now()) /
          context.customNetwork.slotConfig.slotLength,
      ),
    ),
  );
  if (
    BigInt(checkpoint.metadata.inclusionTime) <=
    input.predecessor.header.endTime
  )
    throw new Error("Event is not after the retained predecessor");
  return {
    event,
    policyId: checkpoint.metadata.unit.slice(0, 56),
    assetName: checkpoint.metadata.unit.slice(56),
  };
};

/** The retained ledger output owner, derived from its actual seed phrase. */
export const journeyLedgerOwnerKey = (seedPhrase: string) =>
  CML.PrivateKey.from_bech32(
    walletFromSeed(seedPhrase, { network: "Custom" }).paymentKey,
  );

const ownerKey = (input: JourneyFaultPreparationInput) =>
  journeyLedgerOwnerKey(input.ledgerOwnerSeedPhrase);

/** Publication request shared by live staging and local event verification. */
export type JourneyEventPublicationRequest = {
  name: string;
  identity: string;
  build(): Promise<{ tx: TxSignBuilder; metadata: EventMetadata }>;
};

type EventDeployment = Pick<
  Awaited<ReturnType<typeof publishWorkflowDeploymentOnChain>>,
  "operatorLucid" | "contracts" | "references"
>;

/** The exact withdrawal the SDK publishes for this owner, body and ordinal. */
export const journeyWithdrawalEventRequest = (input: {
  deployment: EventDeployment;
  ownerKey: CML.PrivateKey;
  body: SDK.WithdrawalBody;
  ordinal: number;
}): JourneyEventPublicationRequest => {
  const { deployment, body, ordinal } = input;
  return {
    name: `withdrawal-${ordinal}`,
    identity: SDK.withdrawalBodyBytes(body),
    build: async () => {
      const lucid = deployment.operatorLucid;
      const key = input.ownerKey;
      if (key.to_public().hash().to_hex() !== body.l2_owner)
        throw new Error("Withdrawal signer does not own the retained output");
      const reference = deployment.references.get("withdrawalMint");
      if (reference === undefined)
        throw new Error("Missing published withdrawal minting reference");
      const built = await Effect.runPromise(
        SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
          lucid,
          deployment.contracts,
          {
            body,
            signature: SDK.signWithdrawalBody(key, body),
            refundAddress: await Effect.runPromise(
              SDK.addressDataFromBech32(await lucid.wallet().address()),
            ),
            referenceScripts: { withdrawalMinting: reference },
          },
        ),
      );
      return {
        tx: built.tx,
        metadata: {
          address: built.metadata.withdrawalAddress,
          unit: built.metadata.withdrawalAuthUnit,
          inclusionTime: built.metadata.inclusionTime,
          validTo: built.metadata.validTo,
        },
      };
    },
  };
};

export const publishJourneyWithdrawal = (
  input: JourneyFaultPreparationInput,
  body: SDK.WithdrawalBody,
  ordinal: number,
) =>
  publishEvent(
    input,
    journeyWithdrawalEventRequest({
      deployment: input.context.deployment,
      ownerKey: ownerKey(input),
      body,
      ordinal,
    }),
  );

/** The exact ordinary deposit the SDK publishes for this owner credential. */
export const journeyDepositEventRequest = (input: {
  deployment: EventDeployment;
  ownerKey: CML.PrivateKey;
}): JourneyEventPublicationRequest => {
  const { deployment } = input;
  const address = credentialToAddress("Custom", {
    type: "Key",
    hash: input.ownerKey.to_public().hash().to_hex(),
  });
  return {
    name: "deposit",
    identity: `${address}/10000000/no-datum`,
    build: async () => {
      const reference = deployment.references.get("depositMint");
      if (reference === undefined)
        throw new Error("Missing published deposit minting reference");
      const built = await Effect.runPromise(
        SDK.buildUnsignedDepositTxWithMetadataProgram(
          deployment.operatorLucid,
          deployment.contracts,
          {
            l2Address: address,
            l2Datum: null,
            lovelace: 10_000_000n,
            additionalAssets: {},
            referenceScripts: { depositMinting: reference },
          },
        ),
      );
      return {
        tx: built.tx,
        metadata: {
          address: built.metadata.depositAddress,
          unit: built.metadata.depositAuthUnit,
          inclusionTime: built.metadata.inclusionTime,
          validTo: built.metadata.validTo,
        },
      };
    },
  };
};

export const publishJourneyDeposit = (input: JourneyFaultPreparationInput) =>
  publishEvent(
    input,
    journeyDepositEventRequest({
      deployment: input.context.deployment,
      ownerKey: ownerKey(input),
    }),
  );

export const fabricatedDepositJourneyFixture = createPreparedJourneyFixture(
  "fabricatedDeposit",
  async (input) => {
    const deposit = await publishJourneyDeposit(input);
    return {
      buildFault: (timed) =>
        buildJourneyFabricatedDeposit({ ...timed, deposit }),
      buildSuccessor: (timed) =>
        buildJourneyFabricatedDeposit({ ...timed, deposit, honest: true }),
    };
  },
);

export const createWithdrawalJourneyFixture = (
  category: "fabricatedWithdrawal" | "withdrawalMistag" | "doubleWithdraw",
) =>
  createPreparedJourneyFixture(category, async (input) => {
    const body = journeyWithdrawalBody({
      predecessor: input.predecessor,
      owner: ownerKey(input).to_public().hash().to_hex(),
    });
    const withdrawals = [await publishJourneyWithdrawal(input, body, 0)];
    if (category === "doubleWithdraw")
      withdrawals.push(await publishJourneyWithdrawal(input, body, 1));
    return {
      buildFault: (timed) =>
        buildJourneyWithdrawalEvent({ ...timed, category, withdrawals }),
      buildSuccessor: (timed) =>
        buildJourneyWithdrawalEvent({
          ...timed,
          category,
          withdrawals,
          honest: true,
        }),
    };
  });

export const createWithdrawnInputJourneyFixture = (
  category: "withdrawnInput" | "withdrawnReferenceInput",
) =>
  createPreparedJourneyFixture(category, async (input) => {
    const split = await input.commitHistoryBlock(
      "withdrawal-ledger",
      async (timed) => {
        const selected = ownedHistoryLedgerEntries(
          timed.predecessor,
          ownerKey(input).to_public().hash().to_hex(),
        )[0];
        const entry =
          selected === undefined
            ? undefined
            : [
                selected.outRef.toString("hex"),
                selected.output.toString("hex"),
              ];
        if (entry === undefined)
          throw new Error(
            "Withdrawn-input history requires a deposited output",
          );
        const output = decodeMidgardTxOutput(Buffer.from(entry[1], "hex"));
        const half = output.value.lovelace / 2n;
        const transaction = signHistoryTransaction(
          {
            spendInputs: [Buffer.from(entry[0], "hex")],
            outputs: [
              encodeMidgardTxOutput({
                ...output,
                value: { ...output.value, lovelace: half },
              }),
              encodeMidgardTxOutput({
                ...output,
                value: {
                  lovelace: output.value.lovelace - half,
                  assets: new Map(),
                },
              }),
            ],
            fee: 0n,
            networkId: timed.predecessor.header.expectedNetworkId,
          },
          ownerKey(input),
        );
        const block = await retainHistoryTransactions({
          ...timed,
          transactions: [transaction],
        });
        if (block.replays[0]?.trace.verdict !== "accepted")
          throw new Error("Withdrawal prerequisite split is not valid");
        return block;
      },
    );
    const predecessor = await decodeJourneyRetainedBlock(split);
    const preparation = { ...input, predecessor };
    const body = journeyWithdrawalBody({
      predecessor,
      owner: ownerKey(input).to_public().hash().to_hex(),
    });
    const withdrawal = await publishJourneyWithdrawal(preparation, body, 0);
    return {
      predecessor: split,
      buildFault: (timed) =>
        buildJourneyWithdrawnTransaction({ ...timed, category, withdrawal }),
      buildSuccessor: (timed) =>
        buildJourneyWithdrawnTransaction({
          ...timed,
          category,
          withdrawal,
          honest: true,
        }),
    };
  });
