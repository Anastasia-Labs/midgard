// Zeroes the check times pnpm 11 records for the files a patch adds to a
// package. nixpkgs' pnpm-fixup-state-db zeroes them only for the package's own
// files, so without this every patched dependency makes the pnpm deps
// fixed-output hash change from one build to the next.
// Usage: node pnpm-store-side-effect-times.mjs <@pnpm/store.index module> <store v11 dir>
const [storeIndexModule, storeDir] = process.argv.slice(2);
const { packForStorage, StoreIndex } = await import(storeIndexModule);

const zeroAdded = (effect) =>
  effect?.added instanceof Map
    ? {
        ...effect,
        added: new Map(
          [...effect.added].map(([path, file]) => [
            path,
            { ...file, checkedAt: 0 },
          ]),
        ),
      }
    : effect;

const index = new StoreIndex(storeDir);
const rewritten = [];
for (const [key, data] of index.entries()) {
  if (!(data.sideEffects instanceof Map)) continue;
  const sideEffects = new Map(
    [...data.sideEffects].map(([platform, effect]) => [
      platform,
      zeroAdded(effect),
    ]),
  );
  rewritten.push({ key, buffer: packForStorage({ ...data, sideEffects }) });
}
index.deleteMany(rewritten.map((entry) => entry.key));
index.setRawMany(rewritten);
