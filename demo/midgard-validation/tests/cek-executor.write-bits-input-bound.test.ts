import { Builtin, constT, UPLCConst, type UPLCTerm } from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import { evaluatorCharge } from "./helpers/cek-staged-failure-charge.js";
import { bytes, call, run } from "./helpers/cek-staged-route-terms.js";

/**
 * Protocol version 11 fails writeBits on an input over 4,096 bytes, whatever
 * its indices, after costing it. A program-constant input up to the
 * aggregate direct bound (8,927 bytes beside one index) is proved by the
 * direct failure step, uncharged as every writeBits failure there is; one of
 * 4,096 bytes still succeeds.
 */

const WRITE_BITS = 80n;
const UNCHARGED = { cpu: 0n, memory: 0n };

const indices = (values: readonly bigint[]): UPLCTerm =>
  UPLCConst.listOf(constT.int)(
    values.map((value) => UPLCConst.int(value).value as never),
  );

const writeBits = (length: number, at: readonly bigint[]): UPLCTerm =>
  call(Builtin.writeBits, bytes(length, 1), indices(at), UPLCConst.bool(true));

describe("writeBits over an input past 4,096 bytes", () => {
  it.each([
    ["4,097 bytes at bit 0", 4_097, [0n]],
    ["4,097 bytes with no index", 4_097, []],
    ["5,000 bytes at its last bit", 5_000, [39_999n]],
    ["8,927 bytes, the longest direct input, at bit 0", 8_927, [0n]],
  ] as const)(
    "fails in both modes on %s, proved by the uncharged direct failure step",
    (_, length, at) => {
      const body = writeBits(length, at);
      expect(run(body, "verdict").result.kind).toBe("script_invalid");
      const proof = run(body, "proof");
      expect(proof.result.kind).toBe("script_invalid");
      const last = proof.execution!.steps.at(-1)!;
      expect(last.witness.kind).toBe("executeBuiltinFailure");
      expect("tag" in last.witness ? last.witness.tag : null).toBe(WRITE_BITS);
      expect(last.post.mode).toBe("haltError");
      expect({
        cpu: last.post.cpu - last.pre.cpu,
        memory: last.post.memory - last.pre.memory,
      }).toEqual(UNCHARGED);
      expect(verifyMidgardCekCoreStep(last.pre, last.post, last.witness)).toBe(
        true,
      );
      // The evaluator's charge is the one other post; it is not provable.
      const charge = evaluatorCharge(body);
      expect(charge.cpu > 0n).toBe(true);
      expect(
        verifyMidgardCekCoreStep(
          last.pre,
          {
            ...last.post,
            cpu: last.pre.cpu + charge.cpu,
            memory: last.pre.memory + charge.memory,
          },
          last.witness,
        ),
      ).toBe(false);
    },
  );
});

describe("writeBits over an input of 4,096 bytes", () => {
  it("succeeds in both modes", () => {
    const body = writeBits(4_096, [32_767n]);
    expect(run(body, "verdict").result.kind).toBe("accepted");
    expect(run(body, "proof").result.kind).toBe("accepted");
  });
});
