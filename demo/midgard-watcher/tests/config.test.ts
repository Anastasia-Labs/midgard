import { describe, expect, it } from "vitest";

import {
  parseWatcherConfig,
  parseWatcherConfigJson,
  WATCHER_CONFIG_BOUNDS,
  WATCHER_CONFIG_SCHEMA_VERSION,
  watcherConfigDiagnostic,
  WatcherConfigError,
  type WatcherConfigErrorCode,
} from "../src/config.js";

const PEER_A =
  "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345";
const OPERATOR_ID_A = "11".repeat(32);
const OPERATOR_ID_B = "22".repeat(32);
const GENESIS_ID = "33".repeat(32);

const validConfig = () => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: OPERATOR_ID_A,
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: OPERATOR_ID_B,
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 15,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 15,
      },
    },
  },
  da: {
    peers: [{ identity: "da-peer-a", multiaddr: PEER_A }],
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
    },
  },
  proverWallet: {
    keySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_PROVER_KEY",
    },
  },
  deadlines: {
    daFetchMs: 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

const validLocalNodeConfig = () => {
  const common = validConfig();
  return {
    ...common,
    l1: {
      ...common.l1,
      source: {
        sourceMode: "local_node",
        authorityNodeId: "watcher-node",
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: "/run/cardano/node.socket",
          nodeConfigPath: "/etc/cardano/node-config.json",
          genesisConfigPath: "/etc/cardano/shelley-genesis.json",
          genesisIdentitySha256: GENESIS_ID,
        },
        queryServices: [
          {
            kind: "ogmios",
            identity: "local-ogmios",
            endpoint: "ws://127.0.0.1:1337",
          },
          {
            kind: "kupo",
            identity: "local-kupo",
            endpoint: "http://127.0.0.1:1442",
          },
          {
            kind: "db_sync",
            identity: "local-db-sync",
            endpoint: "postgresql://127.0.0.1:5432/cexplorer",
          },
        ],
      },
    },
  };
};

const rejected = (
  action: () => unknown,
  code: WatcherConfigErrorCode,
  path?: string,
): WatcherConfigError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherConfigError);
    const configError = error as WatcherConfigError;
    expect(configError.code).toBe(code);
    if (path !== undefined) {
      expect(configError.path).toBe(path);
    }
    return configError;
  }
  throw new Error("Expected watcher configuration rejection");
};

describe("strict watcher configuration", () => {
  it("parses and freezes a complete acceptance configuration", () => {
    const parsed = parseWatcherConfig(validConfig());

    expect(parsed).toMatchObject({
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        requestTimeoutMs: 10_000,
        maxConcurrency: 8,
        finality: {
          depth: 15,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 15,
            postFinalityRecoveryMaxDepth: 2_160,
          },
        },
      },
      storage: {
        driver: "sqlite",
        path: "/var/lib/midgard-watcher/watcher.sqlite",
        rollbackAuthorityKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        },
      },
    });
    expect(parseWatcherConfig(parsed)).toBe(parsed);
    expect(parsed.l1.source.sourceMode).toBe("external_providers");
    if (parsed.l1.source.sourceMode !== "external_providers") {
      throw new Error("Expected external provider configuration");
    }
    expect(parsed.l1.source.providers.map(({ identity }) => identity)).toEqual([
      "provider-a",
      "provider-b",
    ]);
    expect(Object.isFrozen(parsed)).toBe(true);
    expect(Object.isFrozen(parsed.l1.source)).toBe(true);
    expect(Object.isFrozen(parsed.l1.source.providers)).toBe(true);
    expect(Object.isFrozen(parsed.proverWallet.keySource)).toBe(true);
    expect(Object.isFrozen(parsed.storage.rollbackAuthorityKeySource)).toBe(
      true,
    );
  });

  it("parses the exact JSON language and an indirect file key source", () => {
    const input = validConfig();
    input.proverWallet.keySource = {
      kind: "file",
      path: "/run/secrets/midgard-watcher-prover.skey",
    } as unknown as typeof input.proverWallet.keySource;

    const parsed = parseWatcherConfigJson(JSON.stringify(input));

    expect(parsed.proverWallet.keySource).toEqual({
      kind: "file",
      path: "/run/secrets/midgard-watcher-prover.skey",
    });
  });

  it("requires a separate durable rollback-authority key source", () => {
    const missing = validConfig();
    delete (missing.storage as Record<string, unknown>)
      .rollbackAuthorityKeySource;
    rejected(
      () => parseWatcherConfig(missing),
      "missing_required_field",
      "$.storage.rollbackAuthorityKeySource",
    );

    const inline = validConfig();
    inline.storage.rollbackAuthorityKeySource =
      "inline-secret" as unknown as typeof inline.storage.rollbackAuthorityKeySource;
    rejected(
      () => parseWatcherConfig(inline),
      "inline_secret_forbidden",
      "$.storage.rollbackAuthorityKeySource",
    );

    const reusedEnvironment = validConfig();
    reusedEnvironment.storage.rollbackAuthorityKeySource = {
      ...reusedEnvironment.proverWallet.keySource,
    };
    rejected(
      () => parseWatcherConfig(reusedEnvironment),
      "secret_source_alias",
      "$.storage.rollbackAuthorityKeySource",
    );

    const reusedFile = validConfig();
    reusedFile.storage.rollbackAuthorityKeySource = {
      kind: "file",
      path: "/run/secrets/shared.skey",
    } as unknown as typeof reusedFile.storage.rollbackAuthorityKeySource;
    reusedFile.proverWallet.keySource = {
      kind: "file",
      path: "/run/secrets/shared.skey",
    } as unknown as typeof reusedFile.proverWallet.keySource;
    rejected(
      () => parseWatcherConfig(reusedFile),
      "secret_source_alias",
      "$.storage.rollbackAuthorityKeySource",
    );
  });

  it("requires two independent external providers in every watcher mode", () => {
    const input = validConfig();
    input.mode = "development";
    input.l1.source.providers = [
      {
        identity: "local-provider",
        operatorIdentitySha256: OPERATOR_ID_A,
        endpoint: "http://127.0.0.1:1442",
      },
    ];

    rejected(
      () => parseWatcherConfig(input),
      "out_of_bounds",
      "$.l1.source.providers",
    );
  });

  it("requires HTTPS transport binding for development external providers", () => {
    const input = validConfig();
    input.mode = "development";
    input.l1.source.providers[0]!.endpoint = "http://127.0.0.1:1442";

    rejected(
      () => parseWatcherConfig(input),
      "invalid_endpoint",
      "$.l1.source.providers[0].endpoint",
    );
  });

  it("admits the exact local-node/Kupmios topology and snapshots it", () => {
    const input = validLocalNodeConfig();
    const parsed = parseWatcherConfig(input);
    expect(parsed.l1.source).toEqual(input.l1.source);
    expect(Object.isFrozen(parsed.l1.source)).toBe(true);
    if (parsed.l1.source.sourceMode !== "local_node") {
      throw new Error("expected admitted local-node source");
    }
    expect(Object.isFrozen(parsed.l1.source.queryServices)).toBe(true);
  });

  it("admits local Kupmios without requiring db-sync", () => {
    const input = validLocalNodeConfig();
    input.l1.source.queryServices = input.l1.source.queryServices.filter(
      (service) => service.kind !== "db_sync",
    );

    const parsed = parseWatcherConfig(input);
    expect(parsed.l1.source.sourceMode).toBe("local_node");
    if (parsed.l1.source.sourceMode !== "local_node") {
      throw new Error("expected admitted local-node source");
    }
    expect(
      parsed.l1.source.queryServices.map((service) => service.kind),
    ).toEqual(["ogmios", "kupo"]);
  });

  it("accepts every adjacent numeric boundary", () => {
    const minimum = validConfig();
    minimum.l1.requestTimeoutMs = WATCHER_CONFIG_BOUNDS.requestTimeoutMs.min;
    minimum.da.requestTimeoutMs = WATCHER_CONFIG_BOUNDS.requestTimeoutMs.min;
    minimum.l1.maxConcurrency = WATCHER_CONFIG_BOUNDS.concurrency.min;
    minimum.da.maxConcurrency = WATCHER_CONFIG_BOUNDS.concurrency.min;
    minimum.l1.finality.depth = WATCHER_CONFIG_BOUNDS.finalityDepth.min;
    minimum.l1.finality.rollback.maxDepth =
      WATCHER_CONFIG_BOUNDS.rollbackDepth.min;
    minimum.deadlines = {
      daFetchMs: WATCHER_CONFIG_BOUNDS.deadlineMs.min,
      daPublishMs: WATCHER_CONFIG_BOUNDS.deadlineMs.min,
      proofConstructMs: WATCHER_CONFIG_BOUNDS.deadlineMs.min,
      proofSubmitMs: WATCHER_CONFIG_BOUNDS.deadlineMs.min,
    };
    expect(parseWatcherConfig(minimum).deadlines.proofSubmitMs).toBe(
      WATCHER_CONFIG_BOUNDS.deadlineMs.min,
    );

    const maximum = validConfig();
    maximum.l1.requestTimeoutMs = WATCHER_CONFIG_BOUNDS.requestTimeoutMs.max;
    maximum.da.requestTimeoutMs = WATCHER_CONFIG_BOUNDS.requestTimeoutMs.max;
    maximum.l1.maxConcurrency = WATCHER_CONFIG_BOUNDS.concurrency.max;
    maximum.da.maxConcurrency = WATCHER_CONFIG_BOUNDS.concurrency.max;
    maximum.l1.finality.depth = WATCHER_CONFIG_BOUNDS.finalityDepth.max;
    maximum.l1.finality.rollback.maxDepth =
      WATCHER_CONFIG_BOUNDS.rollbackDepth.max;
    maximum.deadlines = {
      daFetchMs: WATCHER_CONFIG_BOUNDS.deadlineMs.max,
      daPublishMs: WATCHER_CONFIG_BOUNDS.deadlineMs.max,
      proofConstructMs: WATCHER_CONFIG_BOUNDS.deadlineMs.max,
      proofSubmitMs: WATCHER_CONFIG_BOUNDS.deadlineMs.max,
    };
    expect(parseWatcherConfig(maximum).l1.maxConcurrency).toBe(
      WATCHER_CONFIG_BOUNDS.concurrency.max,
    );
    expect(
      parseWatcherConfig(maximum).l1.finality.rollback
        .postFinalityRecoveryMaxDepth,
    ).toBe(WATCHER_CONFIG_BOUNDS.postFinalityRecoveryDepth.max);
  });

  it.each([
    [
      "l1 timeout zero",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.l1.requestTimeoutMs = 0;
      },
    ],
    [
      "l1 timeout over max",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.l1.requestTimeoutMs =
          WATCHER_CONFIG_BOUNDS.requestTimeoutMs.max + 1;
      },
    ],
    [
      "DA concurrency zero",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.da.maxConcurrency = 0;
      },
    ],
    [
      "L1 concurrency over max",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.l1.maxConcurrency = WATCHER_CONFIG_BOUNDS.concurrency.max + 1;
      },
    ],
    [
      "finality zero",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.l1.finality.depth = 0;
      },
    ],
    [
      "deadline over max",
      "out_of_bounds",
      (input: ReturnType<typeof validConfig>) => {
        input.deadlines.proofConstructMs =
          WATCHER_CONFIG_BOUNDS.deadlineMs.max + 1;
      },
    ],
    [
      "fractional bound",
      "invalid_value",
      (input: ReturnType<typeof validConfig>) => {
        input.da.requestTimeoutMs = 1_000.5;
      },
    ],
    [
      "non-finite bound",
      "invalid_value",
      (input: ReturnType<typeof validConfig>) => {
        input.deadlines.proofSubmitMs = Number.POSITIVE_INFINITY;
      },
    ],
  ] as const)(
    "rejects nonpositive, unbounded, or malformed numbers: %s",
    (_, code, mutate) => {
      const input = validConfig();
      mutate(input);
      rejected(() => parseWatcherConfig(input), code);
    },
  );

  it("requires rollback depth to remain inside finality depth", () => {
    const input = validConfig();
    input.l1.finality.depth = 14;
    input.l1.finality.rollback.maxDepth = 15;

    rejected(
      () => parseWatcherConfig(input),
      "out_of_bounds",
      "$.l1.finality.rollback.maxDepth",
    );
  });

  it("keeps pending rewind depth distinct from the Cardano-k post-finality recovery cap", () => {
    const input = validConfig();
    input.l1.finality.depth = 10;
    input.l1.finality.rollback.maxDepth = 4;

    expect(parseWatcherConfig(input).l1.finality.rollback).toEqual({
      beforeFinality: "rewind",
      afterFinality: "quarantine",
      maxDepth: 4,
      postFinalityRecoveryMaxDepth: 2_160,
    });
  });

  it("requires deadlines to cover their corresponding request timeout", () => {
    const da = validConfig();
    da.da.requestTimeoutMs = 10_001;
    da.deadlines.daFetchMs = 10_000;
    rejected(
      () => parseWatcherConfig(da),
      "out_of_bounds",
      "$.deadlines.daFetchMs",
    );

    const proof = validConfig();
    proof.l1.requestTimeoutMs = 10_001;
    proof.deadlines.proofSubmitMs = 10_000;
    rejected(
      () => parseWatcherConfig(proof),
      "out_of_bounds",
      "$.deadlines.proofSubmitMs",
    );
  });

  it("requires exact target network, mode, schema, and rollback literals", () => {
    const mutations: ReadonlyArray<
      readonly [string, (input: ReturnType<typeof validConfig>) => void]
    > = [
      [
        "schema",
        (input) => {
          input.schemaVersion =
            "midgard-watcher-config-v2" as typeof input.schemaVersion;
        },
      ],
      [
        "mode",
        (input) => {
          input.mode = "production";
        },
      ],
      [
        "network",
        (input) => {
          input.targetNetwork = "preprod";
        },
      ],
      [
        "pre-finality policy",
        (input) => {
          input.l1.finality.rollback.beforeFinality = "ignore";
        },
      ],
      [
        "post-finality policy",
        (input) => {
          input.l1.finality.rollback.afterFinality = "rewind";
        },
      ],
    ];

    for (const [, mutate] of mutations) {
      const input = validConfig();
      mutate(input);
      rejected(() => parseWatcherConfig(input), "invalid_value");
    }
  });

  it("requires two operationally distinct external providers in acceptance mode", () => {
    const one = validConfig();
    one.l1.source.providers = [one.l1.source.providers[0]!];
    rejected(
      () => parseWatcherConfig(one),
      "out_of_bounds",
      "$.l1.source.providers",
    );

    const identityAlias = validConfig();
    identityAlias.l1.source.providers[1]!.identity = "provider-a";
    rejected(
      () => parseWatcherConfig(identityAlias),
      "provider_alias",
      "$.l1.source.providers[1]",
    );

    const operatorAlias = validConfig();
    operatorAlias.l1.source.providers[1]!.operatorIdentitySha256 =
      OPERATOR_ID_A;
    rejected(
      () => parseWatcherConfig(operatorAlias),
      "provider_alias",
      "$.l1.source.providers[1]",
    );

    const endpointAlias = validConfig();
    endpointAlias.l1.source.providers[1]!.endpoint =
      "https://CARDANO-A.EXAMPLE:443/";
    rejected(
      () => parseWatcherConfig(endpointAlias),
      "provider_alias",
      "$.l1.source.providers[1]",
    );

    const trailingDotAlias = validConfig();
    trailingDotAlias.l1.source.providers[1]!.endpoint =
      "https://cardano-a.example./";
    rejected(
      () => parseWatcherConfig(trailingDotAlias),
      "provider_alias",
      "$.l1.source.providers[1]",
    );

    const malformedOperatorIdentity = validConfig();
    malformedOperatorIdentity.l1.source.providers[0]!.operatorIdentitySha256 =
      "11".repeat(31);
    rejected(
      () => parseWatcherConfig(malformedOperatorIdentity),
      "invalid_value",
      "$.l1.source.providers[0].operatorIdentitySha256",
    );
  });

  it.each([
    "http://cardano-a.example",
    "https://localhost:1442",
    "https://127.0.0.1:1442",
    "https://user:password@cardano-a.example",
    "https://cardano-a.example?token=inline",
    "https://cardano-a.example#fragment",
  ])("rejects an unsafe acceptance provider endpoint: %s", (endpoint) => {
    const input = validConfig();
    input.l1.source.providers[0]!.endpoint = endpoint;
    rejected(
      () => parseWatcherConfig(input),
      "invalid_endpoint",
      "$.l1.source.providers[0].endpoint",
    );
  });

  it("rejects unknown, legacy, and mixed L1 source-mode shapes", () => {
    const unknownMode = validConfig();
    (unknownMode.l1.source as Record<string, unknown>).sourceMode =
      "local-provider";
    rejected(
      () => parseWatcherConfig(unknownMode),
      "invalid_value",
      "$.l1.source.sourceMode",
    );

    const legacy = validConfig();
    const legacyL1 = legacy.l1 as unknown as Record<string, unknown>;
    const externalSource = legacyL1.source as Record<string, unknown>;
    delete legacyL1.source;
    legacyL1.providers = externalSource.providers;
    rejected(() => parseWatcherConfig(legacy), "unknown_field", "$.l1");

    const mixedExternal = validConfig();
    Object.assign(mixedExternal.l1.source, { queryServices: [] });
    rejected(
      () => parseWatcherConfig(mixedExternal),
      "unknown_field",
      "$.l1.source",
    );

    const mixedLocal = validLocalNodeConfig();
    Object.assign(mixedLocal.l1.source, {
      providers: validConfig().l1.source.providers,
    });
    rejected(
      () => parseWatcherConfig(mixedLocal),
      "unknown_field",
      "$.l1.source",
    );

    const missingChainSync = validLocalNodeConfig();
    delete (missingChainSync.l1.source as Record<string, unknown>).chainSync;
    rejected(
      () => parseWatcherConfig(missingChainSync),
      "missing_required_field",
      "$.l1.source.chainSync",
    );
  });

  it("rejects every hostile local-node authority mutation", () => {
    const inputs = [
      validLocalNodeConfig(),
      validLocalNodeConfig(),
      validLocalNodeConfig(),
    ];
    inputs[0]!.l1.source.authorityNodeId = "Watcher Node";
    inputs[1]!.l1.source.chainSync.socketPath = "/tmp/node.socket";
    inputs[2]!.l1.source.queryServices[0]!.endpoint = "http://ogmios.example";
    rejected(
      () => parseWatcherConfig(inputs[0]),
      "invalid_value",
      "$.l1.source.authorityNodeId",
    );
    rejected(
      () => parseWatcherConfig(inputs[1]),
      "unsafe_path",
      "$.l1.source.chainSync.socketPath",
    );
    rejected(
      () => parseWatcherConfig(inputs[2]),
      "invalid_endpoint",
      "$.l1.source.queryServices[0].endpoint",
    );
  });

  it("requires bounded, distinct public DA peer multiaddresses", () => {
    const empty = validConfig();
    empty.da.peers = [];
    rejected(() => parseWatcherConfig(empty), "out_of_bounds", "$.da.peers");

    const alias = validConfig();
    alias.da.peers.push({ identity: "da-peer-b", multiaddr: PEER_A });
    rejected(
      () => parseWatcherConfig(alias),
      "provider_alias",
      "$.da.peers[1]",
    );

    for (const multiaddr of [
      "/ip4/203.0.113.4/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      "/dns4/localhost/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      "/dns4/da-a.local/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
      "/dns4/da-a.example/tcp/0/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
    ]) {
      const input = validConfig();
      input.da.peers[0]!.multiaddr = multiaddr;
      rejected(
        () => parseWatcherConfig(input),
        "invalid_endpoint",
        "$.da.peers[0].multiaddr",
      );
    }
  });

  it("rejects ephemeral, relative, aliased, and virtual database paths", () => {
    for (const path of [
      "watcher.sqlite",
      "/tmp/watcher.sqlite",
      "/run/watcher.sqlite",
      "/proc/watcher.sqlite",
      "/var/lib/../tmp/watcher.sqlite",
      "/",
    ]) {
      const input = validConfig();
      input.storage.path = path;
      rejected(
        () => parseWatcherConfig(input),
        "unsafe_path",
        "$.storage.path",
      );
    }
  });

  it("rejects inline wallet material and unsafe key sources", () => {
    const inline = validConfig();
    inline.proverWallet.keySource = "word ".repeat(24) as never;
    rejected(
      () => parseWatcherConfig(inline),
      "inline_secret_forbidden",
      "$.proverWallet.keySource",
    );

    const field = validConfig();
    Object.assign(field.proverWallet.keySource, {
      seedPhrase: "never expose this phrase",
    });
    rejected(
      () => parseWatcherConfig(field),
      "inline_secret_forbidden",
      "$.proverWallet.keySource",
    );

    const unsafeFile = validConfig();
    unsafeFile.proverWallet.keySource = {
      kind: "file",
      path: "/tmp/prover.skey",
    } as unknown as typeof unsafeFile.proverWallet.keySource;
    rejected(
      () => parseWatcherConfig(unsafeFile),
      "unsafe_path",
      "$.proverWallet.keySource.path",
    );

    const unsafeVariable = validConfig();
    unsafeVariable.proverWallet.keySource.variable = "inline-key-value";
    rejected(
      () => parseWatcherConfig(unsafeVariable),
      "invalid_value",
      "$.proverWallet.keySource.variable",
    );
  });

  it("rejects unknown fields at every trust boundary", () => {
    const cases: ReadonlyArray<
      readonly [string, (input: ReturnType<typeof validConfig>) => void]
    > = [
      ["root", (input) => Object.assign(input, { compatibility: true })],
      ["L1", (input) => Object.assign(input.l1, { fallback: true })],
      [
        "provider",
        (input) => Object.assign(input.l1.source.providers[0]!, { alias: "a" }),
      ],
      ["DA", (input) => Object.assign(input.da, { fallback: true })],
      [
        "peer",
        (input) => Object.assign(input.da.peers[0]!, { endpoint: "private" }),
      ],
      ["storage", (input) => Object.assign(input.storage, { autoReset: true })],
      [
        "deadlines",
        (input) => Object.assign(input.deadlines, { unlimited: true }),
      ],
    ];

    for (const [, mutate] of cases) {
      const input = validConfig();
      mutate(input);
      rejected(() => parseWatcherConfig(input), "unknown_field");
    }
  });

  it("requires every root field and rejects accessor-backed input", () => {
    for (const key of Object.keys(validConfig())) {
      const input = validConfig() as Record<string, unknown>;
      delete input[key];
      rejected(
        () => parseWatcherConfig(input),
        "missing_required_field",
        `$.${key}`,
      );
    }

    const accessor = validConfig();
    Object.defineProperty(accessor, "mode", {
      enumerable: true,
      get: () => "acceptance",
    });
    rejected(() => parseWatcherConfig(accessor), "unsafe_value", "$");

    const nestedAccessor = validConfig();
    Object.defineProperty(nestedAccessor.proverWallet.keySource, "kind", {
      enumerable: true,
      get: () => "environment",
    });
    rejected(
      () => parseWatcherConfig(nestedAccessor),
      "unsafe_value",
      "$.proverWallet.keySource",
    );
  });

  it.each([
    "",
    "{",
    "[] trailing",
    '{"schemaVersion":}',
    '{"unterminated":"value}',
    '{"number":01}',
    '{"array":[1,]}',
  ])("rejects malformed JSON without returning parser details: %s", (text) => {
    const error = rejected(
      () => parseWatcherConfigJson(text),
      text.length < WATCHER_CONFIG_BOUNDS.configJsonBytes.min
        ? "out_of_bounds"
        : "malformed_json",
    );
    if (text.length > 0) {
      expect(error.message).not.toContain(text);
    }
  });

  it("rejects duplicate JSON fields before materializing the object", () => {
    const text = JSON.stringify(validConfig()).replace(
      '"mode":"acceptance"',
      '"mode":"acceptance","mode":"development"',
    );

    rejected(() => parseWatcherConfigJson(text), "duplicate_field", "$");
  });

  it("keeps secrets and rejected values out of all diagnostics", () => {
    const secret = "correct horse battery staple";
    const input = validConfig();
    Object.assign(input.proverWallet.keySource, { seed: secret });
    const error = rejected(
      () => parseWatcherConfig(input),
      "inline_secret_forbidden",
    );
    const diagnostic = watcherConfigDiagnostic(error);

    expect(error.message).not.toContain(secret);
    expect(JSON.stringify(diagnostic)).not.toContain(secret);
    expect(diagnostic).toEqual({
      code: "inline_secret_forbidden",
      path: "$.proverWallet.keySource",
      message:
        "Watcher configuration rejected: inline_secret_forbidden at $.proverWallet.keySource",
    });

    const foreign = watcherConfigDiagnostic(new Error(secret));
    expect(JSON.stringify(foreign)).not.toContain(secret);
    expect(foreign.code).toBe("invalid_configuration");
  });
});
