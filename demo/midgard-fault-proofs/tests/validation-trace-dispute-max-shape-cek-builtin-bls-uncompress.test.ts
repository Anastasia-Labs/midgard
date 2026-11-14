/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the operator's thread at the trace's last
 * CEK builtin step, a BLS12-381 G2 uncompress failure at the costliest shape
 * of the uncompress failure arm (core group 12): a point on the twist,
 * outside G2. The successor's thread is at its last direct scalar builtin
 * step, each resolved through its arm chain
 * (validation-trace-dispute-max-shape-case.ts). The same transaction in an
 * honest block, which stamps it TxIsInvalid, starts no workflow and signs
 * nothing; nor does a transaction that uncompresses a valid G2 point, in an
 * honest block that accepts it. */
import { beforeAll, describe, expect, it } from "vitest";

import {
  deepestCase,
  honestBlock,
  LONGEST_FAILING_BUILTIN_SCRIPTS,
  TIMEOUT,
} from "./support/validation-trace-dispute-max-shape-case.js";
import { flatScript } from "./support/validation-trace-dispute-one-step-cek-builtin-drive.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

/**
 * `(program 1.1.0 (lam ctx [(lam _ ()) [(builtin bls12_381_G2_uncompress)
 * G]]))`, G the compressed G2 generator: its uncompress step succeeds and
 * the script returns unit.
 */
const VALID_POINT_SCRIPT = flatScript(
  "010100232498de12456093e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb80001",
);

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.blsUncompress,
    group: 12,
    successor: { group: 3 },
  });

  honestBlock({ failingScript: LONGEST_FAILING_BUILTIN_SCRIPTS.blsUncompress });

  describe("a transaction that uncompresses a valid G2 point, in an honest block", () => {
    let scene: Scene;

    beforeAll(async () => {
      scene = await setUpScene(undefined, {
        passingSpend: { script: VALID_POINT_SCRIPT },
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
        expect(result.kind).toBe("no_fault_detected");
        expect(
          [...scene.l1.signedCbors.keys()].filter((hash) => !signed.has(hash)),
        ).toStrictEqual([]);
        expect(unspent()).toStrictEqual(before);
      },
      TIMEOUT,
    );
  });
});
