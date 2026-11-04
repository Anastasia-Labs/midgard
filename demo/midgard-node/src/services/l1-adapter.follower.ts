/**
 * The role layers over the follower adapter (`./l1-provider.ts`), for
 * `listen` and its workers only. The adapter reads the node's follower store
 * with the node's index (`nodeIndexSpec`), built once from the node's
 * configuration and contracts: the same set its follower records, so a
 * protocol read before the follower's first cursor is the transient
 * `follower:not_initialized`, never a ledger scan. The contracts resolve
 * first, under `FollowerMidgardContractServices`, whose only L1 read (a
 * derived bundle's slot mapping) is a keyed chain-parameter query with no
 * index.
 */
import { Effect, Layer } from "effect";

import {
  L1_INDEX_UNCONFIGURED,
  L1Adapter,
  openAdapterScoped,
  refuseNonFollowerL1,
  refuseRoleL1Access,
} from "./l1-adapter.js";
import {
  nodeIndexFromDeployment,
  type NodeIndexSpec,
} from "./l1-follower.index-spec.js";
import { openNodeL1AccessFromConfig } from "./l1-provider.js";
import { Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  FollowerMidgardContractServices,
  MidgardContracts,
} from "./midgard-contracts.js";

/**
 * The role adapter: the follower store's view of L1 over the node's index.
 * Refuses a non-follower L1 configuration and a missing local node first
 * (`refuseNonFollowerL1`), then an index the deployment cannot give
 * (`l1_index_unconfigured`, also a deterministic `l1_access` failure).
 */
export const FollowerL1AdapterLive = Layer.effect(
  L1Adapter,
  Effect.gen(function* () {
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    let index: NodeIndexSpec | undefined;
    return {
      role: "follower" as const,
      open: (config) =>
        Effect.gen(function* () {
          yield* refuseNonFollowerL1(config);
          const refuse = refuseRoleL1Access(config.NETWORK);
          if (index === undefined) {
            const built = yield* Effect.try({
              try: () => nodeIndexFromDeployment(config, contracts, identity),
              catch: (cause) => refuse(L1_INDEX_UNCONFIGURED, cause),
            });
            if (built.kind === "unconfigured")
              return yield* Effect.fail(
                refuse(L1_INDEX_UNCONFIGURED, new Error(built.detail)),
              );
            index = built.index;
          }
          const opened = index;
          return yield* openAdapterScoped(config, () =>
            openNodeL1AccessFromConfig(config, opened),
          );
        }),
    };
  }),
);

/** The Lucid service of a role process: over the follower adapter. It
 * requires the contract services. */
export const FollowerLucidLive = Lucid.Default.pipe(
  Layer.provide(FollowerL1AdapterLive),
);

/** A role process's Lucid and contract services, the contracts beneath the
 * Lucid so both share one resolution. */
export const FollowerLucidAndContracts = FollowerLucidLive.pipe(
  Layer.provideMerge(FollowerMidgardContractServices),
);
