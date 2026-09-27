import type * as SDK from "@al-ft/midgard-sdk";
import type { Network } from "@lucid-evolution/lucid";
import { Context, Effect, Layer } from "effect";

import type { EventHistorySourceBinding } from "../../src/l1-event-history-source.js";
import type { HistoryTransportOptions } from "../../src/l1-event-history-transport.js";
import { NodeConfig } from "../../src/services/config.js";
import { makeProductionEventHistoryOwner } from "../../src/services/event-history-runtime.js";
import { Lucid } from "../../src/services/lucid.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import {
  ContractDeploymentIdentity,
  type ContractDeploymentIdentityValue,
  MidgardContracts,
} from "../../src/services/midgard-contracts.js";
import { WriteBehind } from "../../src/services/write-behind.js";

/** The binding and transport the production composition shared by listen and
 * acceptance hands its owner, built from a transport configuration naming
 * `ogmiosUrl`. The calling test file must replace makeEventHistoryOwner with an
 * echo of its input (vi.mock), so nothing is started and only the composed
 * owner input is read back. Node services the composition merely forwards to
 * the owner are inert stand-ins. */
export const productionRuntimeHistoryBinding = (input: {
  readonly contracts: SDK.MidgardValidators;
  readonly identity: ContractDeploymentIdentityValue;
  readonly network: Network;
  readonly expectedGenesisLosslessSha256: string;
  readonly ogmiosUrl: string;
}) =>
  Effect.runPromise(
    (
      makeProductionEventHistoryOwner({
        transport: {
          kupoUrl: "http://kupo.invalid:1442",
          ogmiosUrl: input.ogmiosUrl,
          timeoutMs: 1_000,
          blockScanLimit: 1,
          maximumResponseBytes: 1,
          maximumTransactionBytes: 1,
        },
        expectedGenesisLosslessSha256: input.expectedGenesisLosslessSha256,
        heartbeatIntervalMs: 100,
        retainedPointLimit: 1,
        maximumReceiptBytes: 1,
        leaseDurationMs: 60_000,
      }) as unknown as Effect.Effect<
        unknown,
        unknown,
        // The echoed owner needs none of the real owner's SQL, scope or globals.
        | NodeConfig
        | MidgardContracts
        | ContractDeploymentIdentity
        | Lucid
        | MempoolLedgerCache
        | WriteBehind
      >
    ).pipe(
      Effect.map((owner) => {
        const composed = owner as {
          readonly binding?: EventHistorySourceBinding;
          readonly transport?: Omit<HistoryTransportOptions, "signal">;
        };
        if (composed.binding === undefined || composed.transport === undefined)
          throw new Error(
            "makeEventHistoryOwner must be mocked to echo its input",
          );
        return { binding: composed.binding, transport: composed.transport };
      }),
      Effect.provide(
        Layer.mergeAll(
          Layer.succeed(NodeConfig, {
            NETWORK: input.network,
          } as unknown as Context.Tag.Service<typeof NodeConfig>),
          Layer.succeed(
            MidgardContracts,
            MidgardContracts.make({
              ...input.contracts,
              consensusProfile: input.identity.consensusProfile,
            }),
          ),
          Layer.succeed(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make(input.identity),
          ),
          Layer.succeed(Lucid, {
            api: { slotToUnixTime: () => 0 },
          } as unknown as Lucid),
          Layer.succeed(
            MempoolLedgerCache,
            {} as unknown as Context.Tag.Service<typeof MempoolLedgerCache>,
          ),
          Layer.succeed(WriteBehind, {
            flushNow: Effect.void,
          } as unknown as Context.Tag.Service<typeof WriteBehind>),
        ),
      ),
    ),
  );
