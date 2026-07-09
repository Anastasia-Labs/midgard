import { defineConfig, defineDocs } from 'fumadocs-mdx/config';
import { metaSchema, pageSchema } from 'fumadocs-core/source/schema';
import { rehypeCodeDefaultOptions } from 'fumadocs-core/mdx-plugins';
import { transformerTwoslash } from 'fumadocs-twoslash';

// Frontmatter and meta.json schemas: https://fumadocs.dev/docs/mdx/collections
export const docs = defineDocs({
  dir: 'content/docs',
  docs: {
    schema: pageSchema,
    postprocess: {
      includeProcessedMarkdown: true,
    },
  },
  meta: {
    schema: metaSchema,
  },
});

export default defineConfig({
  mdxOptions: {
    rehypeCodeOptions: {
      // Aiken has no Shiki grammar. Use a ```rust fence for Aiken source.
      themes: {
        light: 'github-light',
        dark: 'github-dark',
      },
      // `ts twoslash` blocks are type-checked against the linked SDK packages at
      // build time. A snippet naming an export that does not exist fails the
      // build. Requires demo/midgard-core and demo/lucid-midgard to be built.
      transformers: [
        ...(rehypeCodeDefaultOptions.transformers ?? []),
        transformerTwoslash(),
      ],
    },
  },
});
