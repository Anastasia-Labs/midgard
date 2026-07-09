import { createMDX } from 'fumadocs-mdx/next';

const withMDX = createMDX();

// GitHub Pages serves this repo at /midgard, but local dev serves it at the
// root. Both the router and the static search client need the same value, so
// it is exported to the client as NEXT_PUBLIC_BASE_PATH.
const basePath = process.env.NEXT_PUBLIC_BASE_PATH ?? '';

/** @type {import('next').NextConfig} */
const config = {
  reactStrictMode: true,
  output: 'export',
  trailingSlash: true,
  basePath,
  assetPrefix: basePath,
  images: { unoptimized: true },
  // twoslash resolves types at build time and must not be bundled.
  serverExternalPackages: ['typescript', 'twoslash'],
};

export default withMDX(config);
