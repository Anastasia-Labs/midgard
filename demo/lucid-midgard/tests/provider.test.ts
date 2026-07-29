import { decodeMidgardProofSubmissionV1 } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardTxOutput,
  type MidgardFetch,
  MidgardNodeProvider,
  type MidgardProtocolInfo,
  type OutRef,
  outRefToCbor,
  ProviderCapabilityError,
  ProviderPayloadError,
  ProviderTransportError,
} from "../src/index.js";

const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const outRef: OutRef = {
  txHash: "11".repeat(32),
  outputIndex: 0,
};

const protocolInfo: MidgardProtocolInfo = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: 123456n,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: 44n,
    minFeeB: 155381n,
  },
  submissionLimits: {
    maxSubmitTxCborBytes:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

const protocolInfoJson = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: "123456",
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: "44",
    minFeeB: "155381",
  },
  submissionLimits: {
    maxSubmitTxCborBytes:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

const jsonResponse = (payload: unknown, status = 200): Response =>
  new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json" },
  });

const encodedUtxo = (ref: OutRef = outRef) => ({
  outref: outRefToCbor(ref).toString("hex"),
  outputCbor: encodeMidgardTxOutput(address, {
    lovelace: 2_000_000n,
  }).toString("hex"),
});

const submitTxHex =
  "84018c418041804180002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff8341804180418000";
const submitTx = {
  txHex: submitTxHex,
  txId: computeMidgardNativeTxIdV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(
      Buffer.from(submitTxHex, "hex"),
    ),
  ).toString("hex"),
};

const submitAdmission = (
  payload: Record<string, unknown> = {},
  status = 202,
): Response =>
  jsonResponse(
    { txId: submitTx.txId, status: "queued", duplicate: false, ...payload },
    status,
  );

const makeOtherAddress = (): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(
      CML.PrivateKey.generate_ed25519().to_public().hash(),
    ),
  )
    .to_address()
    .to_bech32();

const makeProvider = (fetchImpl: MidgardFetch): Promise<MidgardNodeProvider> =>
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

describe("MidgardNodeProvider", () => {
  it("accepts only the exact V1 profile and full language surface", async () => {
    const v1ProtocolInfoJson = {
      ...protocolInfoJson,
      apiVersion: 1,
    };
    const provider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000/",
      fetch: async () => jsonResponse(v1ProtocolInfoJson),
    });

    await expect(provider.getProtocolInfo()).resolves.toMatchObject({
      apiVersion: 1,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    });
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000/",
        fetch: async () =>
          jsonResponse({
            ...v1ProtocolInfoJson,
            consensusProfile: {
              ...MIDGARD_CONSENSUS_PROFILE_V1,
              protocolVersion: 2,
            },
          }),
      }),
    ).rejects.toThrow(/does not exactly match/);
  });

  it("rejects direct runtime construction so protocol-info cannot be bypassed", () => {
    const UnsafeConstructor = MidgardNodeProvider as unknown as new (
      options: object,
    ) => MidgardNodeProvider;

    expect(
      () =>
        new UnsafeConstructor({
          endpoint: "http://127.0.0.1:3000",
          fetch: async () => jsonResponse(protocolInfoJson),
        }),
    ).toThrow(ProviderCapabilityError);
  });

  it("fetches and decodes UTxOs by address", async () => {
    const provider = await makeProvider(async (input) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/utxos");
      expect(url.searchParams.get("address")).toBe(address);
      return jsonResponse({ utxos: [encodedUtxo()] });
    });

    await expect(provider.getUtxos(address)).resolves.toMatchObject([
      {
        txHash: outRef.txHash,
        outputIndex: outRef.outputIndex,
        output: {
          address,
          assets: { lovelace: 2_000_000n },
        },
      },
    ]);
  });

  it("rejects malformed UTxO payloads", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({ utxos: [{ outref: "zz", outputCbor: "00" }] }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("wraps malformed UTxO output bytes as provider payload errors", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        utxos: [
          {
            outref: outRefToCbor(outRef).toString("hex"),
            outputCbor: "00",
          },
        ],
      }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("rejects UTxO address-query responses for other addresses", async () => {
    const otherAddress = makeOtherAddress();
    const otherOutput = encodeMidgardTxOutput(otherAddress, {
      lovelace: 2_000_000n,
    }).toString("hex");
    const provider = await makeProvider(async () =>
      jsonResponse({
        utxos: [
          {
            ...encodedUtxo(),
            outputCbor: otherOutput,
          },
        ],
      }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("fetches UTxOs by outref and returns undefined for misses", async () => {
    const hits = await makeProvider(async (input) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/utxo");
      expect(url.searchParams.get("txOutRef")).toBe(
        outRefToCbor(outRef).toString("hex"),
      );
      return jsonResponse({ utxo: encodedUtxo() });
    });
    const miss = await makeProvider(async () =>
      jsonResponse({ error: "missing" }, 404),
    );

    await expect(hits.getUtxoByOutRef(outRef)).resolves.toMatchObject({
      txHash: outRef.txHash,
    });
    await expect(miss.getUtxoByOutRef(outRef)).resolves.toBeUndefined();
  });

  it("rejects substituted UTxOs for outref lookups", async () => {
    const otherOutRef = {
      txHash: "22".repeat(32),
      outputIndex: 0,
    };
    const single = await makeProvider(async () =>
      jsonResponse({ utxo: encodedUtxo(otherOutRef) }),
    );
    await expect(single.getUtxoByOutRef(outRef)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const batch = await makeProvider(async () =>
      jsonResponse({ utxos: [encodedUtxo(otherOutRef)] }),
    );
    await expect(batch.getUtxosByOutRefs([outRef])).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("fetches UTxOs by outref batch", async () => {
    const provider = await makeProvider(async (_input, init) => {
      expect(init?.method).toBe("POST");
      expect(JSON.parse(String(init?.body))).toEqual([`${outRef.txHash}#0`]);
      return jsonResponse({ utxos: [encodedUtxo()] });
    });

    await expect(provider.getUtxosByOutRefs([outRef])).resolves.toHaveLength(1);
  });

  it("accepts the exact current protocol-info shape from the node", async () => {
    const provider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () => jsonResponse(protocolInfoJson),
    });

    await expect(provider.getProtocolInfo()).resolves.toEqual(protocolInfo);
    await expect(provider.getCurrentSlot()).resolves.toBe(123456n);
    await expect(provider.getProtocolParameters()).resolves.toMatchObject({
      minFeeA: 44n,
      minFeeB: 155381n,
      networkId: 0n,
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes,
    });
    expect(provider.diagnostics().protocolInfoSource).toBe("node");
  });

  it("rejects unknown root protocol-info fields", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () =>
          jsonResponse({
            ...protocolInfoJson,
            unknownRootField: true,
          }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });

  it("rejects unknown nested protocol-info fields", async () => {
    const unknownKeyProtocolInfos = [
      {
        ...protocolInfoJson,
        protocolFeeParameters: {
          ...protocolInfoJson.protocolFeeParameters,
          unknownFeeField: true,
        },
      },
      {
        ...protocolInfoJson,
        submissionLimits: {
          ...protocolInfoJson.submissionLimits,
          unknownLimitField: true,
        },
      },
      {
        ...protocolInfoJson,
        validation: {
          ...protocolInfoJson.validation,
          unknownValidationField: true,
        },
      },
      {
        ...protocolInfoJson,
        consensusProfile: {
          ...protocolInfoJson.consensusProfile,
          unknownProfileField: true,
        },
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          {
            ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES[0],
            unknownLanguageField: true,
          },
          ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES.slice(1),
        ],
      },
      {
        ...protocolInfoJson,
        codecSupportedScriptLanguages: [
          {
            ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES[0],
            unknownCodecLanguageField: true,
          },
          ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES.slice(1),
        ],
      },
    ];

    for (const payload of unknownKeyProtocolInfos) {
      await expect(
        MidgardNodeProvider.create({
          endpoint: "http://127.0.0.1:3000",
          fetch: async () => jsonResponse(payload),
        }),
      ).rejects.toBeInstanceOf(ProviderPayloadError);
    }
  });

  it("redacts endpoint credentials and query strings in diagnostics", async () => {
    const provider = await MidgardNodeProvider.create({
      endpoint: "https://user:secret@example.test:8443/base?api_key=hidden",
      fetch: async () => jsonResponse(protocolInfoJson),
    });

    expect(provider.diagnostics().endpoint).toBe(
      "https://example.test:8443/base",
    );
  });

  it("fails closed on incompatible V1 profiles and feature matrices", async () => {
    const malformedProtocolInfos = [
      { ...protocolInfoJson, apiVersion: 99 },
      { ...protocolInfoJson, consensusProfile: undefined },
      {
        ...protocolInfoJson,
        consensusProfile: {
          ...MIDGARD_CONSENSUS_PROFILE_V1,
          protocolVersion: 99,
        },
      },
      {
        ...protocolInfoJson,
        submissionLimits: {
          maxSubmitTxCborBytes:
            MIDGARD_CONSENSUS_PROFILE_V1.limits.maxTxCanonicalCborBytes - 1,
        },
      },
      { ...protocolInfoJson, supportedScriptLanguages: undefined },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [{ name: "PlutusV3", tag: 2 }],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
          { name: "PlutusV2", tag: 1 },
        ],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          { name: "PlutusV3", tag: 2 },
          { name: "MidgardV1", tag: 999 },
        ],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          { name: "PlutusV1", tag: 0 },
          { name: "MidgardV1", tag: 128 },
        ],
      },
    ];

    for (const payload of malformedProtocolInfos) {
      await expect(
        MidgardNodeProvider.create({
          endpoint: "http://127.0.0.1:3000",
          fetch: async () => jsonResponse(payload),
        }),
      ).rejects.toBeInstanceOf(ProviderPayloadError);
    }
  });

  it("derives network ids from supported protocol-info networks", async () => {
    const mainnetProvider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () =>
        jsonResponse({
          ...protocolInfoJson,
          network: "Mainnet",
        }),
    });
    await expect(
      mainnetProvider.getProtocolParameters(),
    ).resolves.toMatchObject({
      networkId: 1n,
    });

    const unsupportedProvider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () =>
        jsonResponse({
          ...protocolInfoJson,
          network: "Experimental",
        }),
    });
    await expect(
      unsupportedProvider.getProtocolParameters(),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });

  it("fails closed when the required protocol-info endpoint is unavailable", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () => new Response("not found", { status: 404 }),
      }),
    ).rejects.toBeInstanceOf(ProviderCapabilityError);
  });

  it("preserves rejected transaction status code and detail", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        txId: outRef.txHash,
        status: "rejected",
        reasonCode: "E_BAD_TX",
        reasonDetail: "bad",
        timestamps: { createdAt: "2026-05-01T00:00:00.000Z" },
      }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).resolves.toEqual({
      kind: "rejected",
      txId: outRef.txHash,
      code: "E_BAD_TX",
      detail: "bad",
      createdAt: "2026-05-01T00:00:00.000Z",
    });
  });

  it("rejects tx-status responses for a different tx id", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        txId: "22".repeat(32),
        status: "accepted",
      }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("classifies unsupported tx-status as a capability error", async () => {
    const provider = await makeProvider(
      async () => new Response("not implemented", { status: 501 }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
  });

  it("submits new and duplicate transactions with durable admission metadata", async () => {
    const responses = [
      submitAdmission({
        firstSeenAt: "2026-05-01T00:00:00.000Z",
        lastSeenAt: "2026-05-01T00:00:00.000Z",
      }),
      submitAdmission(
        {
          firstSeenAt: "2026-05-01T00:00:00.000Z",
          lastSeenAt: "2026-05-01T00:00:01.000Z",
          duplicate: true,
        },
        200,
      ),
    ];
    const submittedBodies: string[] = [];
    const provider = await makeProvider(async (input, init) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/submit");
      expect(init?.method).toBe("POST");
      expect(init?.headers).toMatchObject({
        "content-type": "application/vnd.midgard.v1+cbor",
      });
      const body =
        init?.body instanceof Uint8Array
          ? Buffer.from(init.body)
          : Buffer.from(await new Response(init?.body).arrayBuffer());
      submittedBodies.push(
        decodeMidgardProofSubmissionV1(body).transactionCbor.toString("hex"),
      );
      return responses.shift()!;
    });

    await expect(provider.submitTx(submitTx.txHex)).resolves.toMatchObject({
      txId: submitTx.txId,
      httpStatus: 202,
      duplicate: false,
    });
    await expect(provider.submitTx(submitTx.txHex)).resolves.toMatchObject({
      txId: submitTx.txId,
      httpStatus: 200,
      duplicate: true,
    });
    expect(submittedBodies).toStrictEqual([submitTx.txHex, submitTx.txHex]);
  });

  it("rejects submit responses for a different tx id", async () => {
    const provider = await makeProvider(async () =>
      submitAdmission({ txId: "22".repeat(32) }),
    );

    await expect(provider.submitTx(submitTx.txHex)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("rejects malformed or oversized direct submit payloads before posting", async () => {
    let submitCalls = 0;
    const provider = await makeProvider(async (input) => {
      const url = new URL(String(input));
      if (url.pathname === "/submit") {
        submitCalls += 1;
      }
      return jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          duplicate: false,
        },
        202,
      );
    });

    await expect(provider.submitTx("zz")).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(provider.submitTx("00")).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(
      provider.submitTx(
        "00".repeat(protocolInfo.submissionLimits.maxSubmitTxCborBytes + 1),
      ),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
    expect(submitCalls).toBe(0);
  });

  it("rejects malformed durable submit admission metadata", async () => {
    const malformedResponses = [
      submitAdmission({ txId: "not-a-tx-id" }),
      submitAdmission({ status: "mystery" }),
      submitAdmission({ duplicate: undefined }),
      submitAdmission({ duplicate: true }),
      submitAdmission({}, 200),
      submitAdmission({ status: "accepted" }),
      submitAdmission({ firstSeenAt: 123 }),
      submitAdmission({ lastSeenAt: 123 }),
    ];

    for (const response of malformedResponses) {
      const provider = await makeProvider(async () => response);
      await expect(provider.submitTx(submitTx.txHex)).rejects.toBeInstanceOf(
        ProviderPayloadError,
      );
    }
  });

  it("classifies conflict, backlog, and transport submit failures", async () => {
    const conflict = await makeProvider(async () =>
      jsonResponse({ error: "E_TX_ID_BYTES_CONFLICT" }, 409),
    );
    await expect(conflict.submitTx(submitTx.txHex)).rejects.toMatchObject({
      statusCode: 409,
      retryable: false,
    });

    const backlog = await makeProvider(
      async () => new Response("busy", { status: 503 }),
    );
    await expect(backlog.submitTx(submitTx.txHex)).rejects.toMatchObject({
      statusCode: 503,
      retryable: true,
    });

    const transport = await makeProvider(async () => {
      throw new Error("connection reset");
    });
    await expect(transport.submitTx(submitTx.txHex)).rejects.toBeInstanceOf(
      ProviderTransportError,
    );
  });

  it("rejects invalid protocol current slot strings", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () =>
          jsonResponse({ ...protocolInfoJson, currentSlot: "-1" }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () =>
          jsonResponse({ ...protocolInfoJson, currentSlot: 123456 }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });
});
