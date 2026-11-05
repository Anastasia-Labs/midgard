/**
 * A node on a fresh database has no follower cursor until its follower runs,
 * and its L1 reader answers a protocol address only from the follower's facts
 * (`follower:not_initialized` before the first cursor, never a ledger scan).
 * The startup sequence (`runDaIdentityGatedStartupSequence`) therefore starts
 * following before the DA provider assertions read the on-chain DA threshold:
 * here over the node's real reader (`nodeL1AccessOver`) on a new, migrated
 * database, with `listen`'s follower store as the following step.
 */
import type { LedgerQuery } from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import {
  applyChainSyncEvent,
  applyMigrations,
  type FactStore,
  openPostgresBackend,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateSeedPhrase,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  assertDaThresholdCompatible,
  runDaIdentityGatedStartupSequence,
} from "../src/da/startup.js";
import { NODE_FOLLOWER_SCHEMA } from "../src/database/follower-schema.js";
import {
  nodeFollowerStoreOptions,
  type NodeIndexSpec,
  nodeIndexSpec,
} from "../src/services/l1-follower.index-spec.js";
import {
  type NodeL1Access,
  nodeL1AccessOver,
} from "../src/services/l1-provider.js";
import { fetchDaParamsUtxo } from "../src/transactions/da-attestation.js";
import { emulatorFollowerPlan } from "./helpers/follower-emulator.host.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

/** The configured DA transport threshold. */
const TRANSPORT_THRESHOLD = 2;

const databases = testDatabases();
const stores: FactStore[] = [];
const accesses: NodeL1Access[] = [];
afterEach(async () => {
  await Promise.all(accesses.splice(0).map((access) => access.close()));
  await Promise.all(stores.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

let contracts: SDK.MidgardValidators;
let index: NodeIndexSpec;
beforeAll(async () => {
  contracts = await loadRealMidgardContractsForTest({
    txHash: "11".repeat(32),
    outputIndex: 0,
  });
  index = nodeIndexSpec(
    {
      NETWORK: "Preprod",
      L1_OPERATOR_SEED_PHRASE: generateSeedPhrase(),
      L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
      L1_SETTLEMENT_SEED_PHRASE: "",
      L1_REFERENCE_SCRIPT_SEED_PHRASE: "",
      L1_REFERENCE_SCRIPT_ADDRESS: "",
      L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: "",
    },
    contracts,
    emulatorFollowerPlan({
      contracts,
      network: "Preprod",
      origin: {
        slot: SIM_ORIGIN.point.slot,
        hash: SIM_ORIGIN.point.hash.toString("hex"),
      },
      securityParameter: 6,
    }),
  );
});

/** A local node whose ledger refuses every query: nothing here may scan it. */
const refusingTransport = () => {
  const queries: LedgerQuery[] = [];
  const query = async (query: LedgerQuery): Promise<Uint8Array> => {
    queries.push(query);
    throw new Error(`the ledger refuses ${query.query}`);
  };
  return {
    queries,
    transport: fakeL1NodeTransport({
      query,
      withLedgerState: async (_at, use) => use({ query }),
    }),
  };
};

/** The node's L1 reader on `connectionString`, as `listen` opens it. */
const openNodeReader = async (
  connectionString: string,
  transport: ReturnType<typeof refusingTransport>["transport"],
): Promise<LucidEvolution> => {
  const access = nodeL1AccessOver(transport, {
    index,
    connectionString,
    endpoint: "/dev/null",
    nodeBehindMaxMs: 60_000,
  });
  accesses.push(access);
  return await Lucid(access.provider, "Preprod", {
    presetProtocolParameters: PROTOCOL_PARAMETERS_DEFAULT,
  });
};

/** One block creating the DA params UTxO with `daThreshold` on chain. */
const daParamsBlock = (daThreshold: bigint) => {
  const chain = new SimChain(simUniverse(), SIM_ORIGIN);
  const unit = SDK.daParamsUnit(contracts.daParamsGovernor);
  const datum: SDK.DaParamsDatum = {
    committee: "",
    committee_signers_hash: "00".repeat(32),
    da_threshold: daThreshold,
    owners: ["ab".repeat(28)],
    update_threshold: 1n,
  };
  const { event } = chain.forward([
    {
      inputs: [chain.outsideInput()],
      outputs: [
        {
          address: Buffer.from(
            getAddressDetails(contracts.daParamsGovernor.spendingScriptAddress)
              .address.hex,
            "hex",
          ),
          lovelace: 5_000_000n,
          assets: new Map([
            [unit.slice(0, 56), new Map([[unit.slice(56), 1n]])],
          ]),
          datum: Buffer.from(Data.to(datum, SDK.DaParamsDatum), "hex"),
        },
      ],
      nonce: chain.nonce(),
    },
  ]);
  return event;
};

/** `listen`'s follower on `connectionString`: its first cursor, then the block. */
const follow = (
  connectionString: string,
  event: ReturnType<typeof daParamsBlock>,
  steps: string[],
) =>
  Effect.promise(async () => {
    steps.push("follow");
    const store = openPostgresFactStore({
      ...nodeFollowerStoreOptions(index, 0),
      connection: { connectionString },
    });
    stores.push(store);
    expect((await store.start()).kind).toBe("ready");
    expect((await store.initialize(SIM_ORIGIN)).kind).toBe("initialized");
    expect((await applyChainSyncEvent(store, event)).result.kind).toBe(
      "applied",
    );
  });

/** The startup sequence on a new database whose chain holds `daThreshold`. */
const startOnFreshDatabase = async (daThreshold: bigint) => {
  const connectionString = await databases.create();
  const { queries, transport } = refusingTransport();
  const lucid = await openNodeReader(connectionString, transport);
  const steps: string[] = [];
  const started = Effect.runPromise(
    runDaIdentityGatedStartupSequence({
      localPreflight: Effect.sync(() => {
        steps.push("local_preflight");
        return { transportThreshold: TRANSPORT_THRESHOLD };
      }),
      // A migrated node database: the follower's tables, with no cursor.
      initializeDatabase: Effect.promise(async () => {
        steps.push("database_initialization");
        const backend = openPostgresBackend({ connectionString });
        try {
          await applyMigrations(backend, NODE_FOLLOWER_SCHEMA);
        } finally {
          await backend.close();
        }
      }),
      initializeProtocol: Effect.sync(() => {
        steps.push("protocol_initialization");
      }),
      followL1: follow(connectionString, daParamsBlock(daThreshold), steps),
      providerAssertions: ({ transportThreshold }) =>
        Effect.gen(function* () {
          steps.push("provider_assertions");
          const { datum } = yield* fetchDaParamsUtxo(lucid, contracts);
          steps.push(`on_chain_da_threshold=${datum.da_threshold.toString()}`);
          yield* Effect.try({
            try: () =>
              assertDaThresholdCompatible(
                transportThreshold,
                datum.da_threshold,
              ),
            catch: (cause) => cause,
          });
        }),
    }),
  );
  return { started, steps, queries };
};

describe("DA provider assertions on a fresh node database", () => {
  it("start following first, then read the on-chain DA threshold from the follower", async () => {
    const { started, steps, queries } = await startOnFreshDatabase(2n);
    await expect(started).resolves.toBeUndefined();
    expect(steps).toEqual([
      "local_preflight",
      "database_initialization",
      "protocol_initialization",
      "follow",
      "provider_assertions",
      "on_chain_da_threshold=2",
    ]);
    // Read from the follower's facts, never from the ledger.
    expect(queries).toEqual([]);
  });

  it("still fail startup on an on-chain threshold above the transport's, once following", async () => {
    const { started, steps } = await startOnFreshDatabase(3n);
    await expect(started).rejects.toThrow(
      /DA transport threshold is lower than the on-chain attestation threshold/,
    );
    expect(steps.slice(-3)).toEqual([
      "follow",
      "provider_assertions",
      "on_chain_da_threshold=3",
    ]);
  });
});
