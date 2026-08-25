import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardTxOutput,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1,
  minAdaLovelaceV1,
  outputMeetsMinAdaV1,
} from "../src/value-accounting.js";

/**
 * D-S4 / C49 (#618). The minimum-Ada floor
 * `coins_per_utxo_byte * (160 + serialized canonical output bytes)` exists as
 * two hand-written copies -- `min_ada_lovelace_v1`/`output_meets_min_ada_v1` in
 * `onchain/aiken/lib/midgard/validation-machine-v1.ak` and
 * `minAdaLovelaceV1`/`outputMeetsMinAdaV1` in `../src/value-accounting.ts`.
 * Nothing forced them to agree: both sides' existing boundary tests exercise
 * a single rate and a single output shape, state their slope and comparison
 * legs relative to the formula under test, and pin nothing across the language
 * boundary -- so the two copies could drift apart (or one could ignore the
 * rate entirely) with every per-side assertion still holding.
 *
 * This suite closes that by pinning ABSOLUTE lovelace values. Each vector is a
 * concrete `(coins_per_utxo_byte, serialized_output_bytes)` pair whose floor is
 * written out as a literal, and each is checked three ways: the TypeScript
 * twin, the Aiken twin evaluated from its checked-in source text, and the
 * pinned literal. The boundary legs are pinned as literals too, so the
 * exactly-at accept and one-under reject cannot drift into each other.
 *
 * The two canonical output fixtures are shared with the Aiken side: the same
 * logical outputs, serialized by `encode_midgard_tx_output`, were measured
 * byte-for-byte identical to the TypeScript `encodeMidgardTxOutput` output
 * (41 and 196 bytes; floors 866_310 and 1_534_360 at the C70 snapshot rate).
 * That measurement ran the Aiken twin over these vectors in a scratch copy of
 * `onchain/aiken`, since the Aiken-side absolute pins themselves batch into the
 * regeneration wave (#617) with the rest of the E_MIN_ADA on-chain wiring:
 *
 *   probe_v1_enterprise_two_ada                    41 bytes -> 866_310
 *   probe_v2_base_multiasset_datum_script_ref     196 bytes -> 1_534_360
 *   probe_v3_intercept_and_parameterization         0 bytes -> 689_600
 *
 * Until that wave lands, the Aiken side is bound here through its source: the
 * extracted formula is parsed and evaluated over the same vectors, so an edit
 * to the Aiken slope, intercept, or comparison operator fails this suite.
 */

const read = (path: string): string =>
  readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");

const aikenTwinSource = read(
  "../../../onchain/aiken/lib/midgard/validation-machine-v1.ak",
);
const aikenTwinTestSource = read(
  "../../../onchain/aiken/lib/midgard/validation-machine-v1.test.ak",
);
// #640 relocated the `reject_*` byte constants out of the machine's own body
// and into the canonical rejection-reason module, which the machine now
// imports. The byte pin below therefore reads its definition site, and the
// machine source is held to importing that exact name.
const aikenRejectionReasonSource = read(
  "../../../onchain/aiken/lib/midgard/rejection-reason-v1.ak",
);
// `PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1` is the C70 target-parameter
// snapshot the sibling `value-accounting.test.ts` min-Ada boundary already
// reads, and it is the only in-repo TypeScript pin of the rate. It is read here
// as source text rather than imported so this suite stays independent of the
// heavy emulator import graph that the same helper module also carries.
const targetSnapshotSource = read(
  "./helpers/ordered-collection-boundary-v1.ts",
);
// The two compiled deployment pins (#627 owner ruling, option B). These are the
// constants the ValueAndMint output-descriptor step actually convicts with --
// `env.coins_per_utxo_byte`, held identical across both Aiken environments --
// and they are what this suite binds, rather than the test-only constant it
// used to read. `aiken build --env <name>` compiles exactly one of these into
// every validator, so a rate that differs between them would ship two
// deployments disagreeing about which outputs are fundable.
const aikenEnvDefaultSource = read("../../../onchain/aiken/env/default.ak");
const aikenEnvTestnetSource = read("../../../onchain/aiken/env/testnet.ak");

const parseUnderscoredInt = (text: string): bigint =>
  BigInt(text.replace(/_/gu, ""));

const requireMatch = (
  source: string,
  pattern: RegExp,
  what: string,
): RegExpMatchArray => {
  const match = pattern.exec(source);
  if (match === null) {
    throw new Error(`min-Ada twin cross-check: could not locate ${what}`);
  }
  return match;
};

/**
 * The body of a top-level `pub fn` in an Aiken module, as source text. Aiken
 * formats one statement per line and closes a top-level declaration with a `}`
 * in column zero, so the body is the span between the signature's opening brace
 * and that closing brace. A body containing a nested brace is rejected rather
 * than half-parsed: this extractor only ever needs the two arithmetic twins.
 */
const aikenFunctionBody = (
  source: string,
  functionName: string,
  what: string,
): string => {
  const header = `pub fn ${functionName}(`;
  const headerIndex = source.indexOf(header);
  if (headerIndex < 0) {
    throw new Error(`min-Ada twin cross-check: could not locate ${what}`);
  }
  const openIndex = source.indexOf("{", headerIndex);
  const closeIndex = source.indexOf("\n}", openIndex);
  if (openIndex < 0 || closeIndex < 0) {
    throw new Error(`min-Ada twin cross-check: ${what} is not a closed block`);
  }
  const body = source.slice(openIndex + 1, closeIndex).trim();
  if (body.includes("{")) {
    throw new Error(
      `min-Ada twin cross-check: ${what} is no longer a single expression`,
    );
  }
  return body;
};

const AIKEN_OVERHEAD_BYTES = parseUnderscoredInt(
  requireMatch(
    aikenTwinSource,
    /^pub const min_ada_output_overhead_bytes = ([0-9_]+)$/mu,
    "the Aiken `min_ada_output_overhead_bytes` constant",
  )[1],
);

const AIKEN_FLOOR_BODY = aikenFunctionBody(
  aikenTwinSource,
  "min_ada_lovelace_v1",
  "the Aiken `min_ada_lovelace_v1` body",
);

const AIKEN_PREDICATE_BODY = aikenFunctionBody(
  aikenTwinSource,
  "output_meets_min_ada_v1",
  "the Aiken `output_meets_min_ada_v1` body",
);

const AIKEN_ENV_RATE_PATTERN =
  /^pub const coins_per_utxo_byte: Int = ([0-9_]+)$/mu;

const AIKEN_ENV_DEFAULT_RATE = parseUnderscoredInt(
  requireMatch(
    aikenEnvDefaultSource,
    AIKEN_ENV_RATE_PATTERN,
    "the `coins_per_utxo_byte` pin in onchain/aiken/env/default.ak",
  )[1],
);

const AIKEN_ENV_TESTNET_RATE = parseUnderscoredInt(
  requireMatch(
    aikenEnvTestnetSource,
    AIKEN_ENV_RATE_PATTERN,
    "the `coins_per_utxo_byte` pin in onchain/aiken/env/testnet.ak",
  )[1],
);

// The TypeScript production pin: the mirror carried in the consensus profile,
// imported as a value (not scraped) so a deleted or renamed field is a type
// error rather than a silently skipped assertion.
const TYPESCRIPT_PRODUCTION_RATE = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.coinsPerUtxoByte,
);

// The C70 target-parameter snapshot the pins are provenanced FROM. It is no
// longer the binding this suite convicts on -- it is the third, corroborating
// witness that both production pins still carry the snapshot's value.
const TYPESCRIPT_TARGET_SNAPSHOT_RATE = parseUnderscoredInt(
  requireMatch(
    targetSnapshotSource,
    /^ {2}coinsPerUtxoByte: ([0-9_]+)n,$/mu,
    "the C70 target snapshot's `coinsPerUtxoByte` pin",
  )[1],
);

// ---------------------------------------------------------------------------
// A deliberately tiny evaluator for the extracted Aiken expressions.
//
// It accepts exactly the syntax the two twins are written in -- integer
// literals, identifiers, `+`, `*`, parentheses, a call to
// `min_ada_lovelace_v1`, and one comparison -- and throws on anything else. A
// formula that grows a subtraction, a division, a conditional, or an unknown
// name therefore fails loudly here instead of being silently approximated.
// ---------------------------------------------------------------------------

type Token = string;

const COMPARISON_OPERATORS = new Set([">=", "<=", "==", ">", "<"]);

const tokenizeAiken = (expression: string): Token[] => {
  const tokens: Token[] = [];
  let cursor = 0;
  while (cursor < expression.length) {
    const rest = expression.slice(cursor);
    const match =
      /^(\s+|[0-9][0-9_]*|[a-z_][a-z0-9_]*|>=|<=|==|[+*(),><])/u.exec(rest);
    if (match === null) {
      throw new Error(
        `min-Ada twin cross-check: unsupported Aiken syntax at ${JSON.stringify(
          rest.slice(0, 24),
        )}`,
      );
    }
    const token = match[1];
    if (!/^\s+$/u.test(token)) {
      tokens.push(token);
    }
    cursor += token.length;
  }
  return tokens;
};

type Environment = ReadonlyMap<string, bigint>;

class AikenExpression {
  private index = 0;

  constructor(
    private readonly tokens: readonly Token[],
    private readonly environment: Environment,
    private readonly callFloor: (
      coinsPerUtxoByte: bigint,
      serializedOutputBytes: bigint,
    ) => bigint,
  ) {}

  private peek(): Token | undefined {
    return this.tokens[this.index];
  }

  private take(): Token {
    const token = this.tokens[this.index];
    if (token === undefined) {
      throw new Error("min-Ada twin cross-check: Aiken expression ended early");
    }
    this.index += 1;
    return token;
  }

  private expect(expected: Token): void {
    const token = this.take();
    if (token !== expected) {
      throw new Error(
        `min-Ada twin cross-check: expected ${expected} in the Aiken expression, found ${token}`,
      );
    }
  }

  private atom(): bigint {
    const token = this.take();
    if (token === "(") {
      const value = this.sum();
      this.expect(")");
      return value;
    }
    if (/^[0-9]/u.test(token)) {
      return parseUnderscoredInt(token);
    }
    if (/^[a-z_]/u.test(token)) {
      if (this.peek() === "(") {
        if (token !== "min_ada_lovelace_v1") {
          throw new Error(
            `min-Ada twin cross-check: unexpected Aiken call to ${token}`,
          );
        }
        this.expect("(");
        const coinsPerUtxoByte = this.sum();
        this.expect(",");
        const serializedOutputBytes = this.sum();
        this.expect(")");
        return this.callFloor(coinsPerUtxoByte, serializedOutputBytes);
      }
      const bound = this.environment.get(token);
      if (bound === undefined) {
        throw new Error(
          `min-Ada twin cross-check: unbound Aiken identifier ${token}`,
        );
      }
      return bound;
    }
    throw new Error(
      `min-Ada twin cross-check: unexpected Aiken token ${token}`,
    );
  }

  private product(): bigint {
    let value = this.atom();
    while (this.peek() === "*") {
      this.expect("*");
      value *= this.atom();
    }
    return value;
  }

  private sum(): bigint {
    let value = this.product();
    while (this.peek() === "+") {
      this.expect("+");
      value += this.product();
    }
    return value;
  }

  arithmetic(): bigint {
    const value = this.sum();
    this.exhausted();
    return value;
  }

  predicate(): { readonly operator: Token; readonly value: boolean } {
    const left = this.sum();
    const operator = this.take();
    if (!COMPARISON_OPERATORS.has(operator)) {
      throw new Error(
        `min-Ada twin cross-check: unsupported Aiken comparison ${operator}`,
      );
    }
    const right = this.sum();
    this.exhausted();
    const value =
      operator === ">="
        ? left >= right
        : operator === "<="
          ? left <= right
          : operator === "=="
            ? left === right
            : operator === ">"
              ? left > right
              : left < right;
    return { operator, value };
  }

  private exhausted(): void {
    if (this.index !== this.tokens.length) {
      throw new Error(
        `min-Ada twin cross-check: trailing Aiken tokens ${this.tokens
          .slice(this.index)
          .join(" ")}`,
      );
    }
  }
}

/** The Aiken floor, evaluated from the source text of its own body. */
const aikenFloorFromSource = (
  coinsPerUtxoByte: bigint,
  serializedOutputBytes: bigint,
  options: {
    readonly body?: string;
    readonly overheadBytes?: bigint;
  } = {},
): bigint =>
  new AikenExpression(
    tokenizeAiken(options.body ?? AIKEN_FLOOR_BODY),
    new Map([
      ["coins_per_utxo_byte", coinsPerUtxoByte],
      ["serialized_output_bytes", serializedOutputBytes],
      [
        "min_ada_output_overhead_bytes",
        options.overheadBytes ?? AIKEN_OVERHEAD_BYTES,
      ],
    ]),
    (rate, bytes) => aikenFloorFromSource(rate, bytes, options),
  ).arithmetic();

/** The Aiken acceptance predicate, evaluated from the source text of its body. */
const aikenMeetsMinAdaFromSource = (
  coinsPerUtxoByte: bigint,
  serializedOutputBytes: bigint,
  lovelace: bigint,
  options: { readonly body?: string } = {},
): { readonly operator: Token; readonly value: boolean } =>
  new AikenExpression(
    tokenizeAiken(options.body ?? AIKEN_PREDICATE_BODY),
    new Map([
      ["coins_per_utxo_byte", coinsPerUtxoByte],
      ["serialized_output_bytes", serializedOutputBytes],
      ["lovelace", lovelace],
      ["min_ada_output_overhead_bytes", AIKEN_OVERHEAD_BYTES],
    ]),
    (rate, bytes) => aikenFloorFromSource(rate, bytes),
  ).predicate();

// ---------------------------------------------------------------------------
// The pinned cross-language vectors. Every `floorLovelace` is an absolute
// literal, independently confirmed against the Aiken twin (see the header).
// ---------------------------------------------------------------------------

type MinAdaVector = {
  readonly label: string;
  readonly coinsPerUtxoByte: bigint;
  readonly serializedOutputBytes: bigint;
  readonly floorLovelace: bigint;
};

const MIN_ADA_CROSS_LANGUAGE_VECTORS_V1: readonly MinAdaVector[] = [
  {
    label: "C70 snapshot rate, zero-byte intercept",
    coinsPerUtxoByte: 4_310n,
    serializedOutputBytes: 0n,
    floorLovelace: 689_600n,
  },
  {
    label:
      "C70 snapshot rate, canonical enterprise pub-key output holding 2 ADA",
    coinsPerUtxoByte: 4_310n,
    serializedOutputBytes: 41n,
    floorLovelace: 866_310n,
  },
  {
    label:
      "C70 snapshot rate, canonical base-address output with three assets in two policies, an inline datum and a PlutusV3 script_ref",
    coinsPerUtxoByte: 4_310n,
    serializedOutputBytes: 196n,
    floorLovelace: 1_534_360n,
  },
  {
    label: "unit rate, zero-byte intercept",
    coinsPerUtxoByte: 1n,
    serializedOutputBytes: 0n,
    floorLovelace: 160n,
  },
  {
    label: "unit rate at the 41-byte canonical output",
    coinsPerUtxoByte: 1n,
    serializedOutputBytes: 41n,
    floorLovelace: 201n,
  },
  {
    label: "synthetic 5_000 rate at the 196-byte canonical output",
    coinsPerUtxoByte: 5_000n,
    serializedOutputBytes: 196n,
    floorLovelace: 1_780_000n,
  },
  {
    label: "C70 snapshot rate at a 5_000-byte output",
    coinsPerUtxoByte: 4_310n,
    serializedOutputBytes: 5_000n,
    floorLovelace: 22_239_600n,
  },
] as const;

// The two shared canonical outputs, as the Aiken probe built them: an
// unprotected network-0 enterprise pub-key address over `0xaa * 28`, and the
// same payment credential with an `0xbb * 28` stake credential.
const ENTERPRISE_ADDRESS_V1 = Buffer.concat([
  Buffer.from([0x60]),
  Buffer.alloc(28, 0xaa),
]);

const BASE_ADDRESS_V1 = Buffer.concat([
  Buffer.from([0x00]),
  Buffer.alloc(28, 0xaa),
  Buffer.alloc(28, 0xbb),
]);

const ENTERPRISE_OUTPUT_V1: MidgardTxOutput = {
  address: ENTERPRISE_ADDRESS_V1,
  value: { lovelace: 2_000_000n, assets: new Map() },
};

const RICH_OUTPUT_V1: MidgardTxOutput = {
  address: BASE_ADDRESS_V1,
  value: {
    lovelace: 9_999_999n,
    assets: new Map([
      [
        "11".repeat(28),
        new Map([
          ["abcd", 7n],
          ["ff".repeat(32), 1n],
        ]),
      ],
      ["22".repeat(28), new Map([["", 42n]])],
    ]),
  },
  datum: { kind: "inline", cbor: Buffer.from("d87980", "hex") },
  script_ref: {
    language: "PlutusV3",
    scriptBytes: Buffer.from("4d01000033222220051200120011", "hex"),
  },
};

const ENTERPRISE_OUTPUT_CBOR_HEX_V1 =
  "a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a0";

const RICH_OUTPUT_CBOR_HEX_V1 =
  "a400583900aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb01821a0098967fa2581c11111111111111111111111111111111111111111111111111111111a242abcd075820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff01581c22222222222222222222222222222222222222222222222222222222a140182a0243d879800382034e4d01000033222220051200120011";

describe("min-Ada twin cross-check (D-S4 / C49)", () => {
  it("extracts the Aiken twin as the exact expected source form", () => {
    expect(AIKEN_OVERHEAD_BYTES).toBe(160n);
    expect(AIKEN_OVERHEAD_BYTES).toBe(MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1);
    expect(AIKEN_FLOOR_BODY.replace(/\s+/gu, " ")).toBe(
      "coins_per_utxo_byte * ( min_ada_output_overhead_bytes + serialized_output_bytes )",
    );
    expect(AIKEN_PREDICATE_BODY.replace(/\s+/gu, " ")).toBe(
      "lovelace >= min_ada_lovelace_v1(coins_per_utxo_byte, serialized_output_bytes)",
    );
    // The exactly-at-the-floor accept leg lives entirely in this operator.
    expect(aikenMeetsMinAdaFromSource(4_310n, 41n, 866_310n).operator).toBe(
      ">=",
    );
  });

  it("pins the C70 snapshot rate at 4_310 across both production pins", () => {
    // The two compiled Aiken pins, the TypeScript mirror, and the C70 snapshot
    // they are all provenanced from. Re-pointed 2026-08-23 (#627 owner ruling,
    // option B) off the Aiken test constant and the TypeScript test helper:
    // this suite now convicts on the constants the validators are compiled
    // with, and keeps the snapshot only as a corroborating witness.
    expect(AIKEN_ENV_DEFAULT_RATE).toBe(4_310n);
    expect(AIKEN_ENV_TESTNET_RATE).toBe(4_310n);
    expect(TYPESCRIPT_PRODUCTION_RATE).toBe(4_310n);
    expect(TYPESCRIPT_TARGET_SNAPSHOT_RATE).toBe(4_310n);

    // Held equal to each other, not merely equal to a literal: a coordinated
    // edit that moves every pin at once still has to move them together.
    expect(AIKEN_ENV_TESTNET_RATE).toBe(AIKEN_ENV_DEFAULT_RATE);
    expect(TYPESCRIPT_PRODUCTION_RATE).toBe(AIKEN_ENV_DEFAULT_RATE);
    expect(TYPESCRIPT_TARGET_SNAPSHOT_RATE).toBe(TYPESCRIPT_PRODUCTION_RATE);

    // A zero rate would collapse every floor to zero and silently disable the
    // whole rule; neither side guards against it at runtime, because at a
    // compiled constant it cannot arise at runtime. This is that guard.
    expect(AIKEN_ENV_DEFAULT_RATE > 0n).toBe(true);
    expect(TYPESCRIPT_PRODUCTION_RATE > 0n).toBe(true);
  });

  it("binds the Aiken vectors to the env pin rather than a duplicated literal", () => {
    // The Aiken test module's rate constant must READ the env pin. If it is
    // ever edited back to a literal, its vectors could pass against a rate the
    // validators do not use -- which is exactly the drift this suite exists to
    // catch, and which the old scrape of that literal could not see.
    expect(aikenTwinTestSource).toContain(
      "const target_snapshot_coins_per_utxo_byte = env.coins_per_utxo_byte",
    );
    expect(
      /^const target_snapshot_coins_per_utxo_byte = [0-9_]+$/mu.test(
        aikenTwinTestSource,
      ),
    ).toBe(false);
  });

  it("pins that the env rate is the one the ValueAndMint scan convicts with", () => {
    // The wiring, as source text (#618 ruling 1; R8 of decision 0005). Without
    // these three legs the pins above could agree perfectly while the floor
    // stayed unwired arithmetic, which is the state D-S4 was opened against.
    expect(aikenRejectionReasonSource).toContain(
      'pub const reject_min_ada = #"455f4d494e5f414441"',
    );
    expect(
      /use\s+midgard\/rejection_reason_v1\.\{[^}]*\breject_min_ada\b[^}]*\}/u.test(
        aikenTwinSource,
      ),
    ).toBe(true);
    expect(aikenTwinSource.replace(/\s+/gu, " ")).toContain(
      "if !output_meets_min_ada_v1( env.coins_per_utxo_byte, descriptor.total_length, descriptor.lovelace, )",
    );
    expect(aikenTwinSource.replace(/\s+/gu, " ")).toContain(
      "rejected_successor_is_exact( pre, witness.claimed_successor, reject_min_ada, )",
    );
    // `E_MIN_ADA` is the ASCII preimage of the code the wiring emits: the
    // TypeScript twin must reject with the same wire bytes.
    expect(Buffer.from("455f4d494e5f414441", "hex").toString("ascii")).toBe(
      "E_MIN_ADA",
    );
  });

  it("serializes the two shared canonical outputs to their pinned absolute bytes", () => {
    const enterprise = encodeMidgardTxOutput(ENTERPRISE_OUTPUT_V1);
    expect(enterprise.toString("hex")).toBe(ENTERPRISE_OUTPUT_CBOR_HEX_V1);
    expect(enterprise.length).toBe(41);

    const rich = encodeMidgardTxOutput(RICH_OUTPUT_V1);
    expect(rich.toString("hex")).toBe(RICH_OUTPUT_CBOR_HEX_V1);
    expect(rich.length).toBe(196);

    // The floors those two fixtures fund, as absolute lovelace.
    expect(minAdaLovelaceV1(4_310n, BigInt(enterprise.length))).toBe(866_310n);
    expect(minAdaLovelaceV1(4_310n, BigInt(rich.length))).toBe(1_534_360n);
  });

  for (const vector of MIN_ADA_CROSS_LANGUAGE_VECTORS_V1) {
    it(`agrees on the absolute floor: ${vector.label}`, () => {
      const { coinsPerUtxoByte, serializedOutputBytes, floorLovelace } = vector;

      expect(minAdaLovelaceV1(coinsPerUtxoByte, serializedOutputBytes)).toBe(
        floorLovelace,
      );
      expect(
        aikenFloorFromSource(coinsPerUtxoByte, serializedOutputBytes),
      ).toBe(floorLovelace);
    });

    it(`agrees on the boundary legs: ${vector.label}`, () => {
      const { coinsPerUtxoByte, serializedOutputBytes, floorLovelace } = vector;
      const legs = [
        { lovelace: floorLovelace, accepted: true },
        { lovelace: floorLovelace - 1n, accepted: false },
        { lovelace: floorLovelace + 1n, accepted: true },
        { lovelace: 0n, accepted: floorLovelace === 0n },
      ] as const;

      for (const leg of legs) {
        expect(
          outputMeetsMinAdaV1(
            coinsPerUtxoByte,
            serializedOutputBytes,
            leg.lovelace,
          ),
        ).toBe(leg.accepted);
        expect(
          aikenMeetsMinAdaFromSource(
            coinsPerUtxoByte,
            serializedOutputBytes,
            leg.lovelace,
          ).value,
        ).toBe(leg.accepted);
      }
    });
  }

  it("keeps the Aiken boundary test's accept and reject legs in place", () => {
    const testBody = aikenTwinTestSource.slice(
      aikenTwinTestSource.indexOf(
        "test parameterized_min_ada_boundary_matches_target_snapshot()",
      ),
    );
    expect(
      testBody.indexOf(
        "test parameterized_min_ada_boundary_matches_target_snapshot()",
      ),
    ).toBe(0);
    const scoped = testBody.slice(0, testBody.indexOf("\n}\n"));
    expect(scoped).toContain("output_meets_min_ada_v1(");
    expect(scoped).toContain("!validation_machine_v1.output_meets_min_ada_v1(");
    expect(scoped).toContain("floor - 1");
  });
});

describe("min-Ada twin cross-check hostile controls", () => {
  it("fires when the Aiken intercept constant moves", () => {
    expect(aikenFloorFromSource(4_310n, 41n, { overheadBytes: 161n })).toBe(
      866_310n + 4_310n,
    );
    expect(aikenFloorFromSource(4_310n, 41n, { overheadBytes: 161n })).not.toBe(
      866_310n,
    );
  });

  it("fires when the Aiken slope or an added margin moves", () => {
    expect(
      aikenFloorFromSource(4_310n, 41n, {
        body: "coins_per_utxo_byte * (min_ada_output_overhead_bytes + serialized_output_bytes) + 1",
      }),
    ).toBe(866_311n);
    expect(
      aikenFloorFromSource(4_310n, 41n, {
        body: "coins_per_utxo_byte * (200 + serialized_output_bytes)",
      }),
    ).toBe(1_038_710n);
  });

  it("fires when the Aiken comparison stops admitting the exact floor", () => {
    const strict = aikenMeetsMinAdaFromSource(4_310n, 41n, 866_310n, {
      body: "lovelace > min_ada_lovelace_v1(coins_per_utxo_byte, serialized_output_bytes)",
    });
    expect(strict.operator).toBe(">");
    expect(strict.value).toBe(false);
    expect(aikenMeetsMinAdaFromSource(4_310n, 41n, 866_310n).value).toBe(true);
  });

  it("refuses to evaluate a formula shape it does not understand", () => {
    expect(() =>
      aikenFloorFromSource(4_310n, 41n, {
        body: "coins_per_utxo_byte * (min_ada_output_overhead_bytes - serialized_output_bytes)",
      }),
    ).toThrow(/unsupported Aiken syntax/u);
    expect(() =>
      aikenFloorFromSource(4_310n, 41n, {
        body: "coins_per_utxo_byte * (min_ada_overhead + serialized_output_bytes)",
      }),
    ).toThrow(/unbound Aiken identifier/u);
    expect(() =>
      aikenFloorFromSource(4_310n, 41n, {
        body: "min_fee_a * serialized_output_bytes",
      }),
    ).toThrow(/unbound Aiken identifier/u);
  });

  it("fires when an env pin is absent, reshaped, or disagrees", () => {
    // The extractor must not silently tolerate a missing or renamed env pin --
    // that failure mode would make the whole re-pointing vacuous.
    expect(() =>
      requireMatch(
        "pub const other: Int = 1\n",
        AIKEN_ENV_RATE_PATTERN,
        "the `coins_per_utxo_byte` pin in onchain/aiken/env/default.ak",
      ),
    ).toThrow(/could not locate/u);
    expect(() =>
      requireMatch(
        "pub const coins_per_utxo_byte = 4_310\n",
        AIKEN_ENV_RATE_PATTERN,
        "the `coins_per_utxo_byte` pin in onchain/aiken/env/default.ak",
      ),
    ).toThrow(/could not locate/u);
    // And it must read the value, not merely confirm the line exists: a
    // divergent second environment has to come out as a different number.
    expect(
      parseUnderscoredInt(
        requireMatch(
          "pub const coins_per_utxo_byte: Int = 4_311\n",
          AIKEN_ENV_RATE_PATTERN,
          "a divergent env pin",
        )[1],
      ),
    ).toBe(4_311n);
    expect(4_311n).not.toBe(AIKEN_ENV_DEFAULT_RATE);
  });

  it("refuses to evaluate an absent or reshaped Aiken declaration", () => {
    expect(() =>
      aikenFunctionBody(
        "pub fn other_v1(a: Int) -> Int {\n  a\n}\n",
        "min_ada_lovelace_v1",
        "the Aiken `min_ada_lovelace_v1` body",
      ),
    ).toThrow(/could not locate/u);
    expect(() =>
      aikenFunctionBody(
        "pub fn min_ada_lovelace_v1(a: Int) -> Int {\n  when a is {\n    _ -> a\n  }\n}\n",
        "min_ada_lovelace_v1",
        "the Aiken `min_ada_lovelace_v1` body",
      ),
    ).toThrow(/no longer a single expression/u);
    expect(() =>
      requireMatch(
        "pub const other = 1\n",
        /^pub const min_ada_output_overhead_bytes = ([0-9_]+)$/mu,
        "the Aiken `min_ada_output_overhead_bytes` constant",
      ),
    ).toThrow(/could not locate/u);
  });
});
