import {
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import { RejectCodes, validatePhaseASingle } from "../src/index.js";
import { phaseAConfig } from "./phase-a.outref-projection.js";
import {
  makeNativeTx,
  makeQueued,
  outRefFromByte,
} from "./validation-fixtures.js";

describe("phase A validation", () => {
  it("requires V1 material and rejects a non-canonical profile tuple", async () => {
    const fixture = makeNativeTx();
    const queued = makeQueued(fixture.txId, fixture.txCbor);
    const v1Config = {
      ...phaseAConfig,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    };
    const missing = validatePhaseASingle(
      { ...queued, programMaterialSidecarCbor: undefined },
      v1Config,
    );
    expect(missing).toMatchObject({
      code: RejectCodes.CekProgramMaterial,
    });

    const sidecar = encodeMidgardCekProgramMaterialSidecar([]);
    const accepted = validatePhaseASingle(
      { ...queued, programMaterialSidecarCbor: sidecar },
      v1Config,
    );
    expect("ledgerTx" in accepted).toBe(true);
    if ("ledgerTx" in accepted) {
      expect(accepted.submission.programMaterialSidecarCbor).toEqual(sidecar);
    }

    const unsupportedProfile = validatePhaseASingle(queued, {
      ...phaseAConfig,
      consensusProfile: {
        ...MIDGARD_CONSENSUS_PROFILE,
        protocolVersion: 2,
      } as unknown as typeof MIDGARD_CONSENSUS_PROFILE,
    });
    expect(unsupportedProfile).toMatchObject({
      code: RejectCodes.TxVersion,
    });
  });

  it("rejects unclaimed material unless reference programs remain unresolved", () => {
    const node = { kind: "error" as const };
    const materialSidecar = encodeMidgardCekProgramMaterialSidecar([
      {
        kind: "term",
        root: hashMidgardCekTermNode(node),
        preimage: encodeMidgardCekTermNode(node),
      },
    ]);
    const withoutReferences = makeNativeTx();
    const rejected = validatePhaseASingle(
      {
        ...makeQueued(withoutReferences.txId, withoutReferences.txCbor),
        programMaterialSidecarCbor: materialSidecar,
      },
      phaseAConfig,
    );
    expect(rejected).toMatchObject({
      code: RejectCodes.CekProgramMaterial,
    });

    const unresolvedReference = makeNativeTx({
      referenceInputs: [outRefFromByte(0x72)],
    });
    const deferred = validatePhaseASingle(
      {
        ...makeQueued(unresolvedReference.txId, unresolvedReference.txCbor),
        programMaterialSidecarCbor: materialSidecar,
      },
      phaseAConfig,
    );
    expect("ledgerTx" in deferred).toBe(true);
  });
});
