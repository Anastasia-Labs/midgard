import {
  computeHash28,
  encodeMidgardFieldPreimage,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core";

import { mintItemCborV1 } from "./mint-authorization-emulator.js";

/** Highest-cardinality canonical mint field; every earlier policy has a retained Plutus witness. */
export const mintAuthorizationMaximumMintField = () => {
  const target = mintItemCborV1({
    policyId: Buffer.alloc(28, 0xff),
    assetName: Buffer.alloc(0),
  });
  const authorized: { policyId: Buffer; mint: string; script: string }[] = [];
  for (let index = 0; ; index++) {
    const payload = Buffer.alloc(2);
    payload.writeUInt16BE(index);
    const policyId = computeHash28(Buffer.concat([Buffer.from([3]), payload]));
    const mint = mintItemCborV1({ policyId, assetName: Buffer.alloc(0) });
    if (
      encodeMidgardFieldPreimage([
        ...authorized.map((item) => Buffer.from(item.mint, "hex")),
        Buffer.from(mint, "hex"),
        Buffer.from(target, "hex"),
      ]).length > 32768
    )
      break;
    authorized.push({
      policyId,
      mint,
      script: encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: payload,
      }).toString("hex"),
    });
  }
  authorized.sort((a, b) => Buffer.compare(a.policyId, b.policyId));
  return {
    mintItemCbors: [...authorized.map((item) => item.mint), target],
    scriptWitnessItemCbors: authorized.map((item) => item.script),
    targetPolicyIndex: authorized.length,
  };
};

/** Fills the complete 32KiB witness envelope with distinct minimal Plutus sources. */
export const mintAuthorizationMaximumWitnessField = () => {
  const entries: { bytes: Buffer; hash: Buffer }[] = [];
  const languages = ["PlutusV3", "MidgardV1"] as const;
  let total = 3;
  outer: for (let width = 0; width <= 2; width++) {
    for (let index = 0; index < 256 ** width; index++) {
      const payload = Buffer.alloc(width);
      if (width > 0) payload.writeUIntBE(index, 0, width);
      for (const [languageIndex, language] of languages.entries()) {
        if (width === 2 && language === "MidgardV1") continue;
        const bytes = encodeMidgardVersionedScript({
          language,
          scriptBytes: payload,
        });
        const stride = bytes.length + 1;
        if (total + stride > 32768) break outer;
        entries.push({
          bytes,
          hash: computeHash28(
            Buffer.concat([
              Buffer.from([languageIndex === 0 ? 3 : 128]),
              payload,
            ]),
          ),
        });
        total += stride;
      }
    }
  }
  // Grow the final distinct payload to fill any residual bytes without changing cardinality.
  const remaining = 32768 - total;
  if (remaining > 0) {
    entries.pop();
    const payload = Buffer.alloc(2 + remaining, 0xfe);
    entries.push({
      bytes: encodeMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: payload,
      }),
      hash: computeHash28(Buffer.concat([Buffer.from([3]), payload])),
    });
  }
  entries.sort((a, b) => Buffer.compare(a.hash, b.hash));
  return {
    mintItemCbors: [
      mintItemCborV1({
        policyId: Buffer.alloc(28, 0xff),
        assetName: Buffer.alloc(0),
      }),
    ],
    scriptWitnessItemCbors: entries.map((entry) => entry.bytes.toString("hex")),
  };
};
