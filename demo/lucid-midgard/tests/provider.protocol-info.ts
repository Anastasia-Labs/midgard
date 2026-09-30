import {
  encodeMidgardCekTermNode,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

import {
  encodeMidgardTxOutput,
  type MidgardFetch,
  MidgardNodeProvider,
  type MidgardProtocolInfo,
  type OutRef,
  outRefToCbor,
} from "../src/index.js";

export const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

export const outRef: OutRef = {
  txHash: "11".repeat(32),
  outputIndex: 0,
};

export const deploymentMarker = makeDeploymentMarker("ab".repeat(32));

export const protocolInfo: MidgardProtocolInfo = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: 123456n,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  deploymentMarker,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: 44n,
    minFeeB: 155381n,
  },
  submissionLimits: {
    maxSubmitTxCborBytes:
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

export const protocolInfoJson = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: "123456",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  deploymentMarker,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: "44",
    minFeeB: "155381",
  },
  submissionLimits: {
    maxSubmitTxCborBytes:
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

export const jsonResponse = (payload: unknown, status = 200): Response =>
  new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json" },
  });

export const encodedUtxo = (ref: OutRef = outRef) => ({
  outref: outRefToCbor(ref).toString("hex"),
  outputCbor: encodeMidgardTxOutput(address, {
    lovelace: 2_000_000n,
  }).toString("hex"),
});

const submitTxHex =
  "84018c418041804180002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff8341804180418000";

export const submitTx = {
  txHex: submitTxHex,
  txId: computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(Buffer.from(submitTxHex, "hex")),
  ).toString("hex"),
};

const submitProgramTerm = { kind: "error" } as const;

export const submitProgramMaterial = [
  {
    kind: "term" as const,
    root: hashMidgardCekTermNode(submitProgramTerm),
    preimage: encodeMidgardCekTermNode(submitProgramTerm),
  },
];

export const submitAdmission = (
  payload: Record<string, unknown> = {},
  status = 202,
): Response =>
  jsonResponse(
    { txId: submitTx.txId, status: "queued", duplicate: false, ...payload },
    status,
  );

export const makeOtherAddress = (): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(
      CML.PrivateKey.generate_ed25519().to_public().hash(),
    ),
  )
    .to_address()
    .to_bech32();

export const makeProvider = (
  fetchImpl: MidgardFetch,
): Promise<MidgardNodeProvider> =>
  MidgardNodeProvider.create({
    endpoint: "http://127.0.0.1:3000/",
    fetch: async (input, init) => {
      const url = new URL(String(input));
      if (url.pathname === "/protocol-info") {
        return jsonResponse(protocolInfoJson);
      }
      return fetchImpl(input, init);
    },
  });
