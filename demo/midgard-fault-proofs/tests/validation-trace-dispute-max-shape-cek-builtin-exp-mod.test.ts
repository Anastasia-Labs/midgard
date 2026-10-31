/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the operator's thread at the trace's last
 * CEK builtin step, an expModInteger success at the heaviest shape the
 * transaction's budget admits beside that trace: base, exponent and
 * modulus of 47 64-bit words. The successor's thread is at its last CEK
 * machine step, each resolved through its arm chain
 * (validation-trace-dispute-max-shape-case.ts). The same transaction in an
 * honest block, which stamps it TxIsInvalid, starts no workflow and signs
 * nothing; nor does a transaction that returns after the same
 * expModInteger, in an honest block that accepts it. */
import { beforeAll, describe, expect, it } from "vitest";

import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-block-fixture.js";
import {
  deepestCase,
  honestBlock,
  TIMEOUT,
} from "./support/validation-trace-dispute-max-shape-case.js";
import { flatScript } from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/**
 * `(program 1.1.0 (lam ctx [(lam _ [(lam _ (force (error))) [(builtin
 * expModInteger) b e m]]) (countdown 5147 0)]))`, with m = e =
 * 2^3007 - 1 (47 words) and b = m - 2: the countdown runs to the
 * per-transaction memory cap and the expModInteger to its step cap. The
 * force puts the builtin step where the bisection takes its longest path.
 */
const FAILING = flatScript(
  "01010023232563337ae907d7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8fa41fdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe3e907f7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8f9999191801119801001000918011198010010009112999ab9a3370e0049000080089980199b810024800800520b650480001",
);

/**
 * `(program 1.1.0 (lam ctx [(lam _ ()) [(builtin expModInteger) b e m]]))`,
 * the same b, e and m: the step succeeds and the script returns unit.
 */
const PASSING = flatScript(
  "010100232498ccdeba41f5fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe3e907f7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8fa41fdfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe3e1",
);

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    failingScript: FAILING,
    group: 13,
    successor: { group: 1 },
  });

  honestBlock({ failingScript: FAILING });

  describe("a transaction that returns after the same expModInteger, in an honest block", () => {
    let scene: Scene;

    beforeAll(async () => {
      scene = await setUpScene(undefined, {
        passingSpend: { script: PASSING, exUnits: ADMITTED_TX_EX_UNITS },
      });
    }, TIMEOUT);

    it(
      "starts no workflow and signs nothing",
      async () => {
        const unspent = () =>
          Object.values(scene.h.emulator.ledger)
            .filter(({ spent }) => !spent)
            .map(({ utxo }) => `${utxo.txHash}#${utxo.outputIndex.toString()}`);
        const signed = new Set(scene.l1.signedCbors.keys());
        const before = unspent();
        const result = await makeRunner(scene).runOnce(scene.passing);
        expect(result).toMatchObject({ kind: "no_fault_detected" });
        expect(
          [...scene.l1.signedCbors.keys()].filter((hash) => !signed.has(hash)),
        ).toStrictEqual([]);
        expect(unspent()).toStrictEqual(before);
      },
      TIMEOUT,
    );
  });
});
