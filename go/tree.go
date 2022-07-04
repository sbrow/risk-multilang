package risk

import (
	"fmt"
)

type node struct {
	att   int // Number of attackers.
	def   int // Number of defenders.
	edges []*edge
}

func (n *node) Report(short bool) string {
	s := ""
	if !short {
		s += fmt.Sprintf("Projected Losses: %dv%d\n", n.att, n.def)
	}
	for i, odds := range n.walk() {
		if short {
			s += fmt.Sprintf("%.4f ", odds)
		} else {
			s += fmt.Sprintf("%-2d casualties - %.4f%%\n", i, 100*odds)
		}
	}
	return s[:len(s)-1]
}

func (n *node) walk() []float64 {
	// TODO(sbrow): Fix
	var ch = make(chan []int, 1000)
	for i, node := range n.edges {
		if node != nil {
			walker(ch, node.node, []int{i})
		}
	}
	close(ch)
	sorted := make([]float64, n.att+1)
	for r := range ch {
		lost, path := n.Walk(r...)
		sorted[lost] += path
	}
	return sorted
}

func walker(ch chan []int, curr *node, path []int) {
	if curr == nil {
		return
	}
	for i, node := range curr.edges {
		if node != nil {
			walker(ch, node.node, append(path, i))
		} else {
			if i+1 == len(curr.edges) {
				cpy := make([]int, len(path))
				copy(cpy, path)
				ch <- cpy
			}
		}
	}
}

func (n *node) Walk(path ...int) (att int, p float64) {
	curr := n.edges[path[0]].node
	p = n.edges[path[0]].p
	for _, n := range path[1:] {
		p *= curr.edges[n].p
		curr = curr.edges[n].node
	}
	return n.att - curr.att, p
}

type edge struct {
	node *node
	p    float64
}

func new(att, def int, oneDef bool, mods ...string) *node {
	n := &node{
		att: att,
		def: def,
	}
	if att < 1 || def < 1 {
		n.edges = []*edge{nil}
		return n
	}
	d := def
	if oneDef {
		d = 1
	}
	win, tie, loss := Compare(att, d, mods...)

	switch {
	case att >= 2 && def >= 2 && !oneDef:
		n.edges = []*edge{
			{node: new(att, def-2, oneDef, mods...), p: win},
			{node: new(att-1, def-1, oneDef, mods...), p: tie},
			{node: new(att-2, def, oneDef, mods...), p: loss},
		}
	default:
		n.edges = []*edge{nil, nil}
		if win > 0 {
			n.edges[0] = &edge{node: new(att, def-1, oneDef, mods...), p: win}
		}
		n.edges[1] = &edge{node: new(att-1, def, oneDef, mods...), p: loss}
	}
	return n
}
