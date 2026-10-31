import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { selectRetiredReferenceScriptUtxos } from "../src/transactions/reference-script-sweep.js";
import {
  applyBatch,
  LIVE,
  liveDeployment,
  liveRef,
  liveTarget,
  NO_LIVE_SCRIPTS,
  plainUtxo,
  plan,
  plutusScript,
  refusalCheck,
  refUtxo,
  RETIRED,
  tokenName,
  WALLET,
} from "./reference-script-sweep.plan.js";

describe("retired reference-script sweep scope", () => {
  it("refuses the live deployment's auth policy, in any letter case", () => {
    const utxos = [refUtxo({ index: 1, policyId: LIVE })];
    expect(refusalCheck(() => plan({ utxos, retiredAuthPolicyId: LIVE }))).toBe(
      "live-auth-policy",
    );
    expect(
      refusalCheck(() =>
        plan({ utxos, retiredAuthPolicyId: LIVE.toUpperCase() }),
      ),
    ).toBe("live-auth-policy");
  });

  it("refuses a malformed retired policy id", () => {
    expect(
      refusalCheck(() =>
        plan({ utxos: [], retiredAuthPolicyId: RETIRED.slice(2) }),
      ),
    ).toBe("invalid-retired-policy");
  });

  it("selects only the retired policy's reference-script UTxOs from a mixed wallet", () => {
    const retired = [refUtxo({ index: 1 }), refUtxo({ index: 2 })];
    const live = [
      refUtxo({ index: 3, policyId: LIVE }),
      refUtxo({ index: 4, policyId: LIVE }),
    ];
    const retiredTokenWithoutScript: UTxO = {
      ...refUtxo({ index: 5 }),
      scriptRef: undefined,
    };
    const utxos = [
      plainUtxo(6),
      ...live,
      retiredTokenWithoutScript,
      ...retired,
    ];

    const selected = selectRetiredReferenceScriptUtxos({
      utxos,
      referenceScriptsAddress: WALLET,
      retiredAuthPolicyId: RETIRED,
      live: NO_LIVE_SCRIPTS,
    });
    const sweep = plan({ utxos });

    expect(selected.map((utxo) => utxo.txHash)).toEqual(
      retired.map((utxo) => utxo.txHash),
    );
    expect(sweep.retainedUtxoCount).toBe(4);
    expect(sweep.batches.flatMap((batch) => batch.inputs)).toEqual(retired);
  });

  it("sweeps a retired copy of a live script while the live-policy copy keeps resolving", () => {
    const shared = plutusScript(7, 4_000);
    const target = liveTarget(0, shared);
    const retiredCopy = refUtxo({ index: 1, script: shared });
    const liveCopy = liveRef(2, target);
    const utxos = [retiredCopy, liveCopy, refUtxo({ index: 3 })];

    const sweep = plan({ utxos, live: liveDeployment(target) });

    expect(sweep.batches.flatMap((batch) => batch.inputs)).toEqual([
      retiredCopy,
      utxos[2],
    ]);
    expect(
      sweep.batches.flatMap((batch) => batch.inputs).includes(liveCopy),
    ).toBe(false);
  });

  it("refuses when a live target's only copy is under the retired policy", () => {
    const shared = plutusScript(8, 4_000);
    const stranded = liveTarget(1, shared);
    const resolved = liveTarget(2, plutusScript(9, 4_000));
    const utxos = [refUtxo({ index: 1, script: shared }), liveRef(2, resolved)];

    expect(
      refusalCheck(() =>
        plan({ utxos, live: liveDeployment(stranded, resolved) }),
      ),
    ).toBe("live-target-stranded");
  });

  it("refuses when the only live-policy copy sits at another address", () => {
    const shared = plutusScript(10, 4_000);
    const target = liveTarget(3, shared);
    const utxos = [
      refUtxo({ index: 1, script: shared }),
      liveRef(2, target, "addr_test1elsewhere"),
    ];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-target-stranded");
  });

  it("refuses when the live role copy carries a different script", () => {
    const shared = plutusScript(12, 4_000);
    const target = liveTarget(5, shared);
    const utxos = [
      refUtxo({ index: 1, script: shared }),
      liveRef(2, liveTarget(5, plutusScript(13, 4_000))),
    ];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-target-stranded");
  });

  it("refuses a selected UTxO that the live resolution accepts", () => {
    const script = plutusScript(11, 4_000);
    const target = liveTarget(4, script);
    const accepted: UTxO = {
      ...liveRef(1, target),
      assets: {
        ...liveRef(1, target).assets,
        [toUnit(RETIRED, tokenName(1))]: 1n,
      },
    };
    const utxos = [accepted, liveRef(2, target)];

    expect(
      refusalCheck(() => plan({ utxos, live: liveDeployment(target) })),
    ).toBe("live-resolved-outref");
  });

  it("refuses a retired reference-script UTxO that also holds a live auth token", () => {
    const utxos = [
      refUtxo({
        index: 1,
        extraAssets: { [toUnit(LIVE, tokenName(1))]: 1n },
      }),
    ];

    expect(refusalCheck(() => plan({ utxos }))).toBe("live-auth-token");
  });

  it("refuses a selected UTxO carrying an asset outside the retired policy", () => {
    const utxos = [
      refUtxo({
        index: 1,
        extraAssets: { [toUnit("ab".repeat(28), "00")]: 5n },
      }),
    ];

    expect(refusalCheck(() => plan({ utxos }))).toBe("foreign-asset");
  });

  it("never re-selects quarantine outputs, so a finished sweep plans nothing", () => {
    const utxos = [refUtxo({ index: 1 }), refUtxo({ index: 2 }), plainUtxo(3)];
    const first = plan({ utxos });
    const afterSweep = applyBatch(utxos, first, 0);

    expect(first.batches).toHaveLength(1);
    expect(
      afterSweep.some(
        (utxo) =>
          utxo.scriptRef === undefined &&
          Object.keys(utxo.assets).some((unit) => unit.startsWith(RETIRED)),
      ),
    ).toBe(true);
    expect(plan({ utxos: afterSweep }).batches).toEqual([]);
  });
});
