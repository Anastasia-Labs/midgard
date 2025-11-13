import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  LucidEvolution,
  Maestro,
  PROTOCOL_PARAMETERS_DEFAULT,
  Kupmios,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type LucidContext = {
  lucid: LucidEvolution;
  users: {
    operator: any;
    user1: any;
    user2: any;
  };
  emulator?: Emulator;
};

export type Network = "Mainnet" | "Preprod" | "Preview" | "Custom";

export const NETWORK = (process.env.NETWORK as Network) || "Custom";

/**
 * Creates an emulator context for testing
 */
export const makeEmulatorContext = () =>
  Effect.gen(function* () {
    const users = {
      operator: yield* Effect.sync(() =>
        generateEmulatorAccount({ lovelace: BigInt(100_000_000_000) }),
      ),
      user1: yield* Effect.sync(() =>
        generateEmulatorAccount({ lovelace: BigInt(50_000_000_000) }),
      ),
      user2: yield* Effect.sync(() =>
        generateEmulatorAccount({ lovelace: BigInt(50_000_000_000) }),
      ),
    };

    const emulator = new Emulator([users.operator, users.user1, users.user2], {
      ...PROTOCOL_PARAMETERS_DEFAULT,
      maxTxSize: 23000,
    });

    const lucid = yield* Effect.promise(() => Lucid(emulator, "Custom"));

    // Select operator wallet by default
    lucid.selectWallet.fromSeed(users.operator.seedPhrase);

    return { lucid, users, emulator } as LucidContext;
  });

/**
 * Creates a Maestro context for testnet testing
 */
export const makeMaestroContext = (network: Network) =>
  Effect.gen(function* () {
    const API_KEY = process.env.MAESTRO_API_KEY;
    const OPERATOR_SEED = process.env.OPERATOR_SEED_PHRASE;
    const USER1_SEED = process.env.USER1_SEED_PHRASE;
    const USER2_SEED = process.env.USER2_SEED_PHRASE;

    if (!API_KEY) {
      throw new Error("Missing MAESTRO_API_KEY environment variable");
    }

    if (network === "Custom") {
      throw new Error("Cannot create Maestro context with 'Custom' network");
    }

    const users = {
      operator: {
        seedPhrase: OPERATOR_SEED || "",
      },
      user1: {
        seedPhrase: USER1_SEED || "",
      },
      user2: {
        seedPhrase: USER2_SEED || "",
      },
    };

    const maestro = new Maestro({
      network: network,
      apiKey: API_KEY,
      turboSubmit: false,
    });

    const lucid = yield* Effect.promise(() => Lucid(maestro, network));

    // Select operator wallet by default
    if (users.operator.seedPhrase) {
      lucid.selectWallet.fromSeed(users.operator.seedPhrase);
    }

    return { lucid, users, emulator: undefined } as LucidContext;
  });

/**
 * Creates a Kupmios context for local testing
 */
export const makeKupmiosContext = (network: Network) =>
  Effect.gen(function* () {
    const KUPO_URL = process.env.KUPO_URL || "http://localhost:1442";
    const OGMIOS_URL = process.env.OGMIOS_URL || "ws://localhost:1337";
    const OPERATOR_SEED = process.env.OPERATOR_SEED_PHRASE;
    const USER1_SEED = process.env.USER1_SEED_PHRASE;
    const USER2_SEED = process.env.USER2_SEED_PHRASE;

    const users = {
      operator: {
        seedPhrase: OPERATOR_SEED || "",
      },
      user1: {
        seedPhrase: USER1_SEED || "",
      },
      user2: {
        seedPhrase: USER2_SEED || "",
      },
    };

    const kupmios = new Kupmios(KUPO_URL, OGMIOS_URL);
    const lucid = yield* Effect.promise(() => Lucid(kupmios, network));

    // Select operator wallet by default
    if (users.operator.seedPhrase) {
      lucid.selectWallet.fromSeed(users.operator.seedPhrase);
    }

    return { lucid, users, emulator: undefined } as LucidContext;
  });

/**
 * Creates the appropriate Lucid context based on environment
 */
export const makeLucidContext = (network?: Network) =>
  Effect.gen(function* () {
    const MAESTRO_API_KEY = process.env.MAESTRO_API_KEY;
    const KUPO_URL = process.env.KUPO_URL;
    const selectedNetwork = network ?? NETWORK;

    // Priority: Kupmios > Maestro > Emulator
    if (KUPO_URL && selectedNetwork !== "Custom") {
      console.log(`Using Kupmios provider on ${selectedNetwork}`);
      return yield* makeKupmiosContext(selectedNetwork);
    } else if (MAESTRO_API_KEY && selectedNetwork !== "Custom") {
      console.log(`Using Maestro provider on ${selectedNetwork}`);
      return yield* makeMaestroContext(selectedNetwork);
    } else {
      console.log("Using Emulator");
      return yield* makeEmulatorContext();
    }
  });
