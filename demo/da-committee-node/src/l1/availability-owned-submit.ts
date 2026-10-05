import type { AvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
} from "@al-ft/midgard-core/native-reward-account";
import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  committeeOwnedReadTransports,
  registerCommitteeReadOwner,
} from "../availability/committee-owned-read-transports.js";
import { committeePromiseOwnedStage } from "../availability/promise-execution-scopes.js";
import { committeeScopedFetch } from "../availability/scoped-transports.js";
import type { CommitteeL1ClientConfig } from "../config.js";
import { normalizeNetwork } from "./provider.request-ogmios-descendant-depth.js";

/** A transport refusal leaves the SDK's exact signed intent unresolved. This
 * adapter neither releases its resources nor supplies replacement bytes. */
export const committeeOwnedAvailabilitySubmit = async (args: {
  config: Pick<
    CommitteeL1ClientConfig,
    "network" | "nativeLedger" | "contractDeploymentInfo"
  >;
  kupoUrl: string;
  ogmiosUrl: string;
  journal: AvailabilityOperationJournal;
  actorId: string;
  signedCbor: string;
  breach: (reason: string) => void;
}): Promise<string> => {
  if (
    !args.journal
      .pending(
        String(args.config.contractDeploymentInfo.manifestId),
        args.actorId,
      )
      .some((record) => record.intent.signedCbor === args.signedCbor)
  )
    throw new Error("Owned submit requires the exact persisted pending bytes");
  const scope = createDaAvailabilityReadScope({ attemptTimeoutMs: 3000 });
  const owner = committeeOwnedReadTransports();
  registerCommitteeReadOwner(scope, owner);
  return committeePromiseOwnedStage({
    stage: "submit",
    capMs: 3000,
    breach: args.breach,
    run: async () => {
      try {
        const provider = new NativeLedgerKupmios(
          args.kupoUrl,
          args.ogmiosUrl,
          nativeLedgerAuthoritySource(
            args.config.nativeLedger === undefined
              ? undefined
              : {
                  ...args.config.nativeLedger,
                  network: normalizeNetwork(args.config.network),
                  timeoutMs: 3000,
                },
          ),
          {
            requestTimeoutMs: 3000,
            fetchImpl: committeeScopedFetch(
              scope,
              {
                requestRefusalMs: 3000,
                httpResponseBytes: 4194304,
                webSocketMessageBytes: 4194304,
                rawUtxos: 1024,
              },
              owner.fetchFor(scope),
            ),
          },
        );
        try {
          return await provider.submitTx(args.signedCbor);
        } catch (error) {
          scope.assertCurrent();
          throw error;
        }
      } finally {
        scope.close();
        await owner.drain();
        owner.assertDrained();
      }
    },
  });
};
