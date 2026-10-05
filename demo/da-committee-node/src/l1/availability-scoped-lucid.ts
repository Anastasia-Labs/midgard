import {
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
} from "@al-ft/midgard-core/native-reward-account";
import {
  customSlotConfigFromShelleyGenesisAtWallClock,
  parseOgmiosShelleyGenesisSlotConfig,
} from "@al-ft/midgard-core/ogmios-slot";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import {
  Lucid,
  type LucidEvolution,
  type Provider,
} from "@lucid-evolution/lucid";

import { committeeReadOwner } from "../availability/committee-owned-read-transports.js";
import { committeePromiseOwnedRead } from "../availability/promise-owned-read.js";
import {
  committeeScopedFetch,
  committeeScopedOgmiosRpc,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import type { CommitteeL1ClientConfig } from "../config.js";
import { assertNetworkMagic } from "./provider.ogmios-rpc-session.js";
import {
  getRecord,
  safeSlot,
} from "./provider.parse-persisted-chain-sync-state.js";
import { normalizeNetwork } from "./provider.request-ogmios-descendant-depth.js";
import { selectL1SubmitterWallet } from "./submitter.js";

/** Each isolated library request uses the original scope and an owned instance
 * fetch. The caller joins that owner before handing off or releasing its actor. */
export const committeeAttemptProvider = (args: {
  config: CommitteeL1ClientConfig;
  kupoUrl: string;
  ogmiosUrl: string;
  scope: DaAvailabilityReadScope;
  requestRefusalMs: number;
  limits: CommitteeSourceReadLimits;
}): Provider => {
  const owner = committeeReadOwner(args.scope);
  if (!owner) throw new Error("Scoped Lucid HTTP ownership is unavailable");
  const fetchImpl = committeeScopedFetch(
    args.scope,
    args.limits,
    owner.fetchFor(args.scope),
  );
  const create = () => {
    args.scope.assertCurrent();
    const timeoutMs = Math.max(
      1,
      Math.ceil(Math.min(args.requestRefusalMs, args.scope.remainingMs())),
    );
    return new NativeLedgerKupmios(
      args.kupoUrl,
      args.ogmiosUrl,
      nativeLedgerAuthoritySource(
        args.config.nativeLedger === undefined
          ? undefined
          : {
              ...args.config.nativeLedger,
              network: normalizeNetwork(args.config.network),
              timeoutMs,
            },
      ),
      { requestTimeoutMs: timeoutMs, awaitTxTimeoutMs: timeoutMs, fetchImpl },
    );
  };
  const initial = create();
  return new Proxy(initial, {
    get(target, property) {
      if (property === "submitTx")
        return () => {
          throw new Error(
            "Unsigned attempt provider cannot submit transactions",
          );
        };
      const value = Reflect.get(target, property, target) as unknown;
      if (typeof value !== "function") return value;
      return (...params: unknown[]) =>
        committeePromiseOwnedRead(args.scope)(async () => {
          const provider = create();
          const method = Reflect.get(provider, property, provider) as (
            ...input: unknown[]
          ) => Promise<unknown>;
          return Reflect.apply(method, provider, params);
        });
    },
  });
};

/** A timed-out builder owns this Lucid instance and cannot repin the next
 * attempt's cache. Shared signed submission/reconciliation uses its own client. */
export const committeeScopedAttemptLucid = async (args: {
  config: CommitteeL1ClientConfig;
  original: LucidEvolution;
  kupoUrl: string;
  ogmiosUrl: string;
  scope: DaAvailabilityReadScope;
  limits: CommitteeSourceReadLimits;
}): Promise<LucidEvolution> => {
  const { scope, config } = args;
  const provider = committeeAttemptProvider({
    ...args,
    requestRefusalMs: args.limits.requestRefusalMs,
  });
  let slotConfig = args.original.config().slotConfig;
  if (config.network === "Custom") {
    const rpc = await committeeScopedOgmiosRpc(
      args.ogmiosUrl,
      scope,
      args.limits,
    );
    try {
      const raw = getRecord(
        await rpc.request("queryNetwork/genesisConfiguration", {
          era: "shelley",
        }),
        "Scoped attempt Shelley genesis",
      );
      assertNetworkMagic(
        config.network,
        safeSlot(
          raw.networkMagic ?? raw.network_magic,
          "Scoped attempt network magic",
        ),
        "Ogmios",
        config.cardanoL1Source.networkMagic,
      );
      const genesis = parseOgmiosShelleyGenesisSlotConfig({ result: raw });
      slotConfig = customSlotConfigFromShelleyGenesisAtWallClock(genesis, {
        nowMs: Date.now(),
      });
    } finally {
      rpc.close();
    }
  }
  const lucid = await committeePromiseOwnedRead(scope)(() =>
    Lucid(provider, normalizeNetwork(config.network), {
      evaluator: args.original.config().evaluator,
      slotConfig,
    }),
  );
  if (!config.availabilitySubmitterKeySource)
    throw new Error("Scoped responder actor key is unavailable");
  // Key selection mutates only this isolated attempt; never the shared client.
  await selectL1SubmitterWallet(lucid, config.availabilitySubmitterKeySource);
  scope.assertCurrent();
  return lucid;
};
