import { source } from '@/lib/source';
import { createFromSource } from 'fumadocs-core/search/server';

// Static export has no server to query, so the index is emitted at build time
// and downloaded by the client on first search.
export const revalidate = false;

export const { staticGET: GET } = createFromSource(source, {
  // https://docs.orama.com/docs/orama-js/supported-languages
  language: 'english',
});
