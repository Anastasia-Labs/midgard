import {
  applyDoubleCborEncoding,
  credentialToAddress,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  fetchReferenceScriptUtxosProgram,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT,
  referenceScriptAuthUnit,
  type ReferenceScriptProviderRead,
  type ReferenceScriptTarget,
} from "../src/reference-scripts.js";
import { StateQueueError } from "../src/state-queue.js";

const policyId = "ab".repeat(28);
const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "cd".repeat(28),
});
const elsewhere = credentialToAddress("Preprod", {
  type: "Key",
  hash: "ef".repeat(28),
});
const script = (seed: number): Script => ({
  type: "PlutusV3",
  script: applyDoubleCborEncoding(
    `5820${seed.toString(16).padStart(2, "0").repeat(32)}`,
  ),
});
const targets = (count: number): ReferenceScriptTarget[] =>
  Object.keys(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, count)
    .map((name, index) => ({ name, script: script(index) }));
const holder = (
  target: ReferenceScriptTarget,
  txHash: string,
  outputIndex = 0,
  overrides: Partial<UTxO> = {},
): UTxO => ({
  txHash,
  outputIndex,
  address,
  assets: {
    lovelace: 40_000_000n,
    [referenceScriptAuthUnit(policyId, target.name)]: 1n,
  },
  scriptRef: target.script,
  ...overrides,
});

/** A provider serving `wallet` for both the wallet read and the per-unit
 * read, in the order given, whatever address it is asked about. */
const provider = (wallet: readonly UTxO[]) => {
  const reads = { wallet: 0, unit: 0 };
  const lucid = {
    utxosAt: async () => {
      reads.wallet++;
      return [...wallet];
    },
    utxosAtWithUnit: async (_: string, unit: string) => {
      reads.unit++;
      return wallet.filter((utxo) => utxo.assets[unit] !== undefined);
    },
  } as unknown as LucidEvolution;
  return { lucid, reads };
};

const resolve = (
  wallet: readonly UTxO[],
  wanted: readonly ReferenceScriptTarget[],
) => {
  const { lucid, reads } = provider(wallet);
  return Effect.runPromise(
    Effect.either(
      fetchReferenceScriptUtxosProgram(lucid, address, wanted, { policyId }),
    ),
  ).then((result) => ({ result, reads }));
};

describe("reference-script resolution picks one holder deterministically", () => {
  const perTarget = targets(1);
  const wide = targets(REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT + 1);
  const cases = [
    ["per-target read", perTarget, { wallet: 0, unit: 1 }],
    ["wallet read", wide, { wallet: 1, unit: 0 }],
  ] as const;

  it.each(cases)(
    "picks the lowest outRef of the accepted holders on the %s, in any provider order",
    async (_path, wanted, expectedReads) => {
      const target = wanted[0]!;
      const low = holder(target, "bb".repeat(32), 3);
      const high = holder(target, "bb".repeat(32), 7);
      const higher = holder(target, "cc".repeat(32), 0);
      const others = wanted
        .slice(1)
        .map((other, index) =>
          holder(other, (index + 1).toString(16).padStart(64, "0")),
        );
      for (const order of [
        [higher, high, low],
        [low, higher, high],
        [high, low, higher],
      ]) {
        const { result, reads } = await resolve([...order, ...others], wanted);
        expect(reads).toEqual(expectedReads);
        expect(result._tag).toBe("Right");
        if (result._tag === "Right") {
          expect(result.right[0]).toEqual({ name: target.name, utxo: low });
          expect(result.right.slice(1).map(({ utxo }) => utxo)).toEqual(others);
        }
      }
    },
  );
});

describe("reference-script resolution refuses an imposter holder", () => {
  const [target] = targets(1) as [ReferenceScriptTarget];
  const withoutRole = holder(target, "11".repeat(32), 0, {
    assets: { lovelace: 40_000_000n },
  });
  const otherScript = holder(target, "22".repeat(32), 0, {
    scriptRef: script(200),
  });
  const otherAddress = holder(target, "33".repeat(32), 0, {
    address: elsewhere,
  });
  const doubleRole = holder(target, "44".repeat(32), 0, {
    assets: {
      lovelace: 40_000_000n,
      [referenceScriptAuthUnit(policyId, target.name)]: 2n,
    },
  });
  const genuine = holder(target, "ff".repeat(32));
  const imposters = [withoutRole, otherScript, otherAddress, doubleRole];

  it.each([
    ["per-target read", [target]],
    [
      "wallet read",
      [target, ...targets(REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT + 1).slice(1)],
    ],
  ] as const)(
    "resolves only the genuine holder on the %s",
    async (_path, wanted) => {
      const others = wanted
        .slice(1)
        .map((other, index) =>
          holder(other, (index + 1).toString(16).padStart(64, "0")),
        );
      const refused = await resolve([...imposters, ...others], wanted);
      expect(refused.result._tag).toBe("Left");
      if (refused.result._tag === "Left") {
        expect(refused.result.left.message).toBe("Missing reference script");
        expect(String(refused.result.left.cause)).toContain(target.name);
      }
      const accepted = await resolve(
        [...imposters, genuine, ...others],
        wanted,
      );
      expect(accepted.result._tag).toBe("Right");
      if (accepted.result._tag === "Right")
        expect(accepted.result.right[0]!.utxo).toBe(genuine);
    },
  );
});

type ResolutionRefusal = {
  readonly message: string;
  readonly cause: unknown;
  readonly reason?: string;
  readonly retryable?: boolean;
};

/** A provider whose reads serve `snapshots` in turn, the last one for every
 * later read, as Kupo does while it re-applies blocks after a rollback. */
const rewinding = (snapshots: readonly (readonly UTxO[])[]) => {
  const reads = { wallet: 0, unit: 0 };
  const serve = () => [
    ...snapshots[Math.min(reads.wallet + reads.unit, snapshots.length) - 1]!,
  ];
  const lucid = {
    utxosAt: async () => {
      reads.wallet++;
      return serve();
    },
    utxosAtWithUnit: async (_: string, unit: string) => {
      reads.unit++;
      return serve().filter((utxo) => utxo.assets[unit] !== undefined);
    },
  } as unknown as LucidEvolution;
  return { lucid, reads };
};

/** A caller's retry policy: re-runs a step only while its failure declares
 * itself retryable, as the node's provider retry does. */
const retryWhileRetryable =
  (maxAttempts: number): ReferenceScriptProviderRead =>
  (_label, read) =>
    Effect.gen(function* () {
      for (let attempt = 1; ; attempt++) {
        const result = yield* Effect.either(read);
        if (result._tag === "Right") return result.right;
        const { retryable } = result.left as ResolutionRefusal;
        if (retryable !== true || attempt >= maxAttempts)
          return yield* Effect.fail(result.left);
      }
    });

const resolveRewinding = (
  snapshots: readonly (readonly UTxO[])[],
  wanted: readonly ReferenceScriptTarget[],
  providerRead?: ReferenceScriptProviderRead,
) => {
  const { lucid, reads } = rewinding(snapshots);
  return Effect.runPromise(
    Effect.either(
      fetchReferenceScriptUtxosProgram(
        lucid,
        address,
        wanted,
        { policyId },
        providerRead,
      ),
    ),
  ).then((result) => ({ result, reads }));
};

describe("reference-script resolution retries a holder the provider has not indexed yet", () => {
  const wide = targets(REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT + 1);
  const holders = (wanted: readonly ReferenceScriptTarget[]) =>
    wanted.map((target, index) =>
      holder(target, (index + 1).toString(16).padStart(64, "0")),
    );
  const [first] = targets(1) as [ReferenceScriptTarget];
  const imposter = holder(first, "22".repeat(32), 0, {
    scriptRef: script(200),
  });
  const paths = [
    ["per-target read", targets(1), "unit"],
    ["wallet read", wide, "wallet"],
  ] as const;

  it.each(paths)(
    "resolves once the holder appears on the %s",
    async (_path, wanted, counter) => {
      const genuine = holders(wanted);
      const { result, reads } = await resolveRewinding(
        [genuine.slice(1), genuine],
        wanted,
        retryWhileRetryable(4),
      );
      expect(result._tag).toBe("Right");
      if (result._tag === "Right")
        expect(result.right.map(({ utxo }) => utxo)).toEqual(genuine);
      expect(reads[counter]).toBe(2);
    },
  );

  it("resolves under a policy that retries every failure", async () => {
    const wanted = targets(1);
    const retryAny: ReferenceScriptProviderRead = (_label, read) =>
      Effect.retry(read, { times: 3 });
    const { result, reads } = await resolveRewinding(
      [[], holders(wanted)],
      wanted,
      retryAny,
    );
    expect(result._tag).toBe("Right");
    expect(reads.unit).toBe(2);
  });

  it.each(paths)(
    "still refuses an imposter after the budget on the %s",
    async (_path, wanted, counter) => {
      const others = holders(wanted).slice(1);
      const { result, reads } = await resolveRewinding(
        [[imposter, ...others]],
        wanted,
        retryWhileRetryable(3),
      );
      expect(reads[counter]).toBe(3);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        const refusal = result.left as ResolutionRefusal;
        expect(refusal).toBeInstanceOf(StateQueueError);
        expect(refusal.message).toBe("Missing reference script");
        expect(String(refusal.cause)).toContain(first.name);
        expect(refusal.reason).toBe("reference-script-not-indexed");
        expect(refusal.retryable).toBe(true);
      }
    },
  );

  it("reads once and refuses on the default policy", async () => {
    const wanted = targets(1);
    const { result, reads } = await resolveRewinding(
      [[], holders(wanted)],
      wanted,
    );
    expect(reads.unit).toBe(1);
    expect(result._tag).toBe("Left");
    if (result._tag === "Left")
      expect((result.left as ResolutionRefusal).reason).toBe(
        "reference-script-not-indexed",
      );
  });

  it.each(paths)(
    "refuses an unknown target without reading or retrying on the %s",
    async (_path, wanted, counter) => {
      const unknown = { name: "no such script", script: script(201) };
      const { result, reads } = await resolveRewinding(
        [holders(wanted)],
        [...wanted, unknown],
        retryWhileRetryable(4),
      );
      expect(reads[counter]).toBe(0);
      expect(result._tag).toBe("Left");
      if (result._tag === "Left") {
        const refusal = result.left as ResolutionRefusal;
        expect(refusal).toBeInstanceOf(StateQueueError);
        expect(refusal.message).toBe("Unknown reference-script target");
        expect(refusal.reason).toBe("reference-script-unknown-target");
        expect(refusal.retryable).toBe(false);
      }
    },
  );
});
