import { defineConfig } from 'tsup';

export default defineConfig({
  entry: ['src/bin.ts'],
  clean: true,
  publicDir: true,
  treeshake: 'smallest',
  external: ['@parcel/watcher'],
  format: ['esm'],
  noExternal: ['@anastasia-labs/cardano-multiplatform-lib-nodejs'],
  platform: 'node',
  target: 'node18',
});
