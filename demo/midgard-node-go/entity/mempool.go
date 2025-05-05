package entity

import (
	"slices"
	"sync"
)

type Mempool struct {
	mu   sync.Mutex
	txs  []Transaction
	seen map[string]struct{}
}

func NewMempool() *Mempool {
	return &Mempool{
		txs:  []Transaction{},
		seen: make(map[string]struct{}),
	}
}

func (m *Mempool) Add(tx Transaction) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if _, exists := m.seen[tx.ID]; exists {
		return
	}
	m.seen[tx.ID] = struct{}{} 
	m.txs = append(m.txs, tx)
}

func (m *Mempool) All() []Transaction {
	m.mu.Lock()
	defer m.mu.Unlock()
	return slices.Clone(m.txs)
}
