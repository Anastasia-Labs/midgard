import type { CML } from "@lucid-evolution/lucid";

/** Adds the body's minted (and burned) quantities to the inputs' side. */
export const addMintedAssets = (
  inputAssets: Map<string, bigint>,
  mint: CML.Mint | undefined,
): void => {
  if (mint === undefined) return;
  const policies = mint.keys();
  for (let i = 0; i < policies.len(); i += 1) {
    const policyHash = policies.get(i);
    const assets = mint.get_assets(policyHash)!;
    const names = assets.keys();
    for (let j = 0; j < names.len(); j += 1) {
      const name = names.get(j),
        unit = policyHash.to_hex() + name.to_hex();
      inputAssets.set(unit, (inputAssets.get(unit) ?? 0n) + assets.get(name)!);
    }
  }
};
