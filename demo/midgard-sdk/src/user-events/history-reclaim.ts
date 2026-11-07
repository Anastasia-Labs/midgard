import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { scriptRewardAddress } from "../cardano-addresses.js";
import { type MidgardValidators } from "../common.js";
import { completeWithFinalLayoutProgram } from "../reserve-payout/completion.js";
import { ReservePayoutTxError } from "../reserve-payout/errors.js";
import { fetchHubOracleReferenceProgram } from "../reserve-payout/hub-reference.js";
import { selectFeeInputProgram } from "../reserve-payout/inputs.js";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
} from "../tx-context-redeemer.js";
import {
  EventHistoryData,
  type EventHistoryKind,
  EventHistoryReclaim,
} from "./history-data.js";
import {
  eventHistoryDeploymentFromContracts,
  requireEventHistoryContracts,
} from "./history-deployment.js";
import {
  authenticateHistoryNodes,
  readEventHistoryOrders,
  selectHistoryWitness,
} from "./history-query.js";

export type ReclaimEventHistoryDataConfig = {
  readonly kind: EventHistoryKind;
  readonly retainedInput: UTxO;
  readonly hubOracleRefInput: UTxO;
  readonly feeInput?: UTxO;
  readonly retentionScriptReference?: UTxO;
  /** A script owner authorizes through its exact zero withdrawal. */
  readonly scriptAuthorization?: {
    readonly script: Script;
    readonly redeemer: string;
    readonly referenceInput?: UTxO;
  };
};

/** Separate reclamation after authenticated absence. The retained datum fixes
 * the authorization credential; neither an archive nor a caller's key does. */
export const buildReclaimEventHistoryDataTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: ReclaimEventHistoryDataConfig,
) =>
  Effect.gen(function* () {
    const prepared = yield* Effect.tryPromise({
      try: async () => {
        const network = lucid.config().network;
        if (network === undefined)
          throw new Error("Missing reclamation network");
        const pair = requireEventHistoryContracts(contracts);
        const history =
          config.kind === "Deposit" ? pair.deposit : pair.withdrawal;
        const [retained] = await lucid.utxosByOutRef([config.retainedInput]);
        if (
          retained?.datum == null ||
          retained.address !== history.retention.spendingScriptAddress ||
          retained.scriptRef != null
        )
          throw new Error(
            "Retained event data is unavailable at the deployed retention script",
          );
        if (retained.datum !== config.retainedInput.datum)
          throw new Error("Retained datum differs from the requested output");
        const datum = Data.from(retained.datum, EventHistoryData);
        const hub = await Effect.runPromise(
          fetchHubOracleReferenceProgram(
            lucid,
            contracts,
            config.hubOracleRefInput,
          ),
        );
        const deployment = eventHistoryDeploymentFromContracts(history);
        const list = await lucid.utxosAt(deployment.address);
        // Require a complete provider snapshot before choosing absence. Opening
        // remaining external Orders also ensures their current carriage exists.
        readEventHistoryOrders(
          list,
          await lucid.utxosAt(deployment.retentionAddress),
          deployment,
        );
        const anchor = selectHistoryWitness(
          authenticateHistoryNodes(list, deployment),
          datum.event_key,
        );
        if (
          anchor.key === datum.event_key &&
          anchor.node.payload !== "RootContent" &&
          "Order" in anchor.node.payload
        )
          throw new Error(
            "Event is still present; retained data cannot be reclaimed",
          );
        const authorization = config.scriptAuthorization;
        if ("PublicKeyCredential" in datum.reclaim_auth) {
          if (authorization !== undefined)
            throw new Error(
              "Key reclamation cannot substitute script authorization",
            );
        } else if (
          authorization === undefined ||
          validatorToScriptHash(authorization.script) !==
            datum.reclaim_auth.ScriptCredential[0]
        )
          throw new Error(
            "Reclamation requires the exact retained script credential",
          );
        if (
          config.retentionScriptReference !== undefined &&
          (config.retentionScriptReference.scriptRef == null ||
            validatorToScriptHash(config.retentionScriptReference.scriptRef) !==
              validatorToScriptHash(history.retention.spendingScript))
        )
          throw new Error("Wrong retention reference script");
        if (
          authorization?.referenceInput !== undefined &&
          (authorization.referenceInput.scriptRef == null ||
            validatorToScriptHash(authorization.referenceInput.scriptRef) !==
              validatorToScriptHash(authorization.script))
        )
          throw new Error("Wrong reclamation authorization reference script");
        const references = [
          hub,
          anchor.utxo,
          config.retentionScriptReference,
          authorization?.referenceInput,
        ].filter((utxo): utxo is UTxO => utxo !== undefined);
        const uniqueReferences = [
          ...new Map(
            references.map((utxo) => [outRefLabel(utxo), utxo]),
          ).values(),
        ];
        if (
          uniqueReferences.some(
            (reference) => outRefLabel(reference) === outRefLabel(retained),
          )
        )
          throw new Error("Reclamation cannot consume its absence witness");
        return {
          hub,
          network,
          history,
          retained,
          datum,
          anchor,
          references: uniqueReferences,
        };
      },
      catch: (cause) =>
        new ReservePayoutTxError({
          message: "Failed to resolve retained-data reclamation",
          cause,
        }),
    });
    const { hub, network, history, retained, datum, anchor, references } =
      prepared;
    const feeInput = yield* selectFeeInputProgram(lucid, config.feeInput, [
      retained,
      ...references,
    ]);
    let layout:
      | {
          retainedInputIndex: bigint;
          absenceReferenceIndex: bigint;
          hubReferenceIndex: bigint;
        }
      | undefined;
    return yield* completeWithFinalLayoutProgram({
      label: "event history data reclamation",
      lucid,
      walletInputExclusions: [retained, feeInput, ...references],
      resolveLayout: () => {
        if (layout === undefined)
          throw new Error("Unresolved reclamation layout");
        return layout;
      },
      makeTx: () => {
        let tx = lucid
          .newTx()
          .readFrom(references)
          .collectFrom([feeInput])
          .collectFrom([retained], ((ctx) => {
            requireOwnSpendPurpose(ctx, retained, "retained data");
            layout = {
              retainedInputIndex: requireInputIndex(
                ctx,
                retained,
                "retained data",
              ),
              absenceReferenceIndex: requireReferenceInputIndex(
                ctx,
                anchor.utxo,
                "history absence",
              ),
              hubReferenceIndex: requireReferenceInputIndex(
                ctx,
                hub,
                "hub oracle",
              ),
            };
            return Data.to(
              {
                absence_reference_index: layout.absenceReferenceIndex,
                hub_reference_index: layout.hubReferenceIndex,
              },
              EventHistoryReclaim,
            );
          }) satisfies BuildTxWithRedeemer);
        if (config.retentionScriptReference === undefined)
          tx = tx.attach.Script(history.retention.spendingScript);
        if ("PublicKeyCredential" in datum.reclaim_auth)
          tx = tx.addSignerKey(datum.reclaim_auth.PublicKeyCredential[0]);
        else {
          const auth = config.scriptAuthorization!;
          tx = tx.withdraw(
            scriptRewardAddress(network, auth.script),
            0n,
            auth.redeemer,
          );
          if (auth.referenceInput === undefined)
            tx = tx.attach.Script(auth.script);
        }
        return tx;
      },
    });
  });
