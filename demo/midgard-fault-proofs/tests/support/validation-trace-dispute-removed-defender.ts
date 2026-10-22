/** A defender leaving both operator lists mid-case, by real L1 events on the
 * Open scene: it retires voluntarily, out of the active list into the
 * retired list, and once its bond unlocks it recovers that bond, which takes
 * it out of the retired list as well. */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { publishRemovalReferenceScripts } from "./emulator/reference-scripts.js";
import { retireOperator } from "./rotated-successor-operator.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

const directory = async ({ h }: Scene) =>
  await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(h.funderLucid, h.contracts),
  );

/** Which operator lists hold `operatorKeyHash` now. */
export const operatorListsHolding = async (
  scene: Scene,
  operatorKeyHash: string,
): Promise<readonly ("active" | "retired")[]> => {
  const { active, retired } = await directory(scene);
  return [
    ...(SDK.findNodeByKey(active, operatorKeyHash) === undefined
      ? []
      : (["active"] as const)),
    ...(SDK.findNodeByKey(retired, operatorKeyHash) === undefined
      ? []
      : (["retired"] as const)),
  ];
};

/**
 * Retires the operator `operatorKeyHash`, whose wallet `lucid` holds, then
 * recovers its bond once it unlocks, from the scene's emulator now.
 */
export const removeOperatorFromBothLists = async (
  scene: Scene,
  operatorKeyHash: string,
  lucid: LucidEvolution,
): Promise<void> => {
  const { h } = scene;
  const removalReferenceScriptPublications =
    await publishRemovalReferenceScripts({
      lucid: h.funderLucid,
      contracts: h.contracts,
    });
  await retireOperator(
    { ...h, removalReferenceScriptPublications } as never,
    operatorKeyHash,
    lucid as never,
  );
  const witnesses = SDK.deriveRecoverOperatorBondWitnesses({
    snapshot: await directory(scene),
    contracts: h.contracts,
    operatorKeyHash,
  });
  // Recovery's lower bound must be strictly after the bond's unlock time.
  const unlock = witnesses.bondUnlockTime ?? 0n;
  while (BigInt(h.emulator.now()) - 1_000n <= unlock) h.emulator.awaitSlot(1);
  const { published } = removalReferenceScriptPublications;
  const { tx } = await Effect.runPromise(
    SDK.buildUnsignedRecoverOperatorBondTxProgram({
      lucid,
      contracts: h.contracts,
      operatorKeyHash,
      retiredOperatorScriptRefs: [
        {
          name: "retired-operators spending",
          utxo: published.retiredOperatorsSpend,
        },
        {
          name: "retired-operators minting",
          utxo: published.retiredOperatorsMint,
        },
      ],
      retiredNode: witnesses.retiredNode,
      retiredAnchor: witnesses.retiredAnchor,
      retiredNodeUnit: witnesses.retiredNodeUnit,
      validFrom: BigInt(h.emulator.now()) - 1_000n,
      validTo: BigInt(h.emulator.now()) + 60_000n,
    }),
  );
  await lucid.awaitTx(await (await tx.sign.withWallet().complete()).submit());
};
