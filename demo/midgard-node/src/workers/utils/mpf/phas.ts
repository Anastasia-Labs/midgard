import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { MidgardMpf, MpfError } from "../mpf.js";

const toPhasTrieItem = (keyCbor: Buffer, valueCbor: Buffer) => ({
  key: Buffer.from(keyCbor),
  value: Buffer.from(valueCbor),
});

export const keyValuePhasRoot = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Effect.Effect<string, MpfError, never> =>
  Effect.gen(function* () {
    if (keys.length !== values.length) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build PHAS root for ${keys.length} keys and ${values.length} values`,
          ),
        ),
      );
    }
    if (keys.length === 0) {
      return SDK.EMPTY_MERKLE_TREE_ROOT;
    }
    const mpf = yield* MidgardMpf.createScratch("phas-root");
    yield* mpf.applyBatch(
      keys.map((key, index) => {
        const item = toPhasTrieItem(key, values[index]!);
        return {
          type: "insert" as const,
          key: item.key,
          value: item.value,
        };
      }),
    );
    return yield* mpf.rootHex();
  });

export const keyValuePhasProof = (
  keys: readonly Buffer[],
  values: readonly Buffer[],
  key: Buffer,
): Effect.Effect<SDK.Proof, MpfError, never> =>
  Effect.gen(function* () {
    if (keys.length !== values.length) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `Cannot build PHAS proof for ${keys.length} keys and ${values.length} values`,
          ),
        ),
      );
    }
    if (keys.length === 0) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error("Cannot build a PHAS membership proof for an empty tree"),
        ),
      );
    }
    const mpf = yield* MidgardMpf.createScratch("phas-proof");
    yield* mpf.applyBatch(
      keys.map((itemKey, index) => {
        const item = toPhasTrieItem(itemKey, values[index]!);
        return {
          type: "insert" as const,
          key: item.key,
          value: item.value,
        };
      }),
    );
    const proof = yield* mpf.prove(key);
    return yield* Effect.try({
      try: () =>
        LucidData.from(
          proof.cbor.toString("hex"),
          SDK.Proof as never,
        ) as SDK.Proof,
      catch: (e) => MpfError.phasRoot(e),
    });
  });
