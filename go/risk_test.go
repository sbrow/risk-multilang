package risk

import (
	"fmt"
	"math/rand"
	"sort"
	"testing"
	"time"

	"github.com/sbrow/prob/dice"
)

func init() {
	_, _ = dice.NewTable(dice.New(dice.D6()))
	_, _ = dice.NewTable(dice.New(dice.D6(), dice.D6(), dice.D6()))
	_, _ = dice.NewTable(dice.New(dice.D6(), dice.D6()))
}

var comparetests = []struct {
	att  int
	def  int
	want string
}{
	{2, 1, "0.5787 0.0000 0.4213"},
	{3, 1, "0.6597 0.0000 0.3403"},
	{2, 2, "0.2276 0.3241 0.4483"},
}

func TestCompare(t *testing.T) {
	for i, tt := range comparetests {
		t.Run(fmt.Sprint(i), func(t *testing.T) {
			win, loss, tie := Compare(tt.att, tt.def)
			s := fmt.Sprintf("%.4f %.4f %.4f", win, loss, tie)
			if s != tt.want {
				t.Errorf("got %q, want %q", s, tt.want)
			}
		})
	}
}

var walktests = []struct {
	att    int
	def    int
	oneDef bool
	mods   []string
	want   string
}{
	{1, 1, false, []string{}, "0.4167 0.5833"},
	{1, 1, true, []string{}, "0.4167 0.5833"},
	{2, 1, false, []string{}, "0.5787 0.1755 0.2458"},
	{2, 1, true, []string{}, "0.5787 0.1755 0.2458"},
	{3, 1, false, []string{}, "0.6597 0.1969 0.0597 0.0836"},
	{3, 1, true, []string{}, "0.6597 0.1969 0.0597 0.0836"},
	{2, 2, false, []string{}, "0.2276 0.1350 0.6373"},
	// {2, 2, true, []string{}, "0.2276 0.1350 0.6373"},
	{2, 2, true, []string{"bunker", "bunker", "bunker", "bunker", "bunker"},
		"0.0000 0.0000 1.0000"},
}

func TestCompleteWalk(t *testing.T) {
	for i, tt := range walktests {
		t.Run(fmt.Sprint(i), func(t *testing.T) {
			node := new(tt.att, tt.def, tt.oneDef, tt.mods...)
			s := fmt.Sprint(node.Report(true))
			if s != tt.want {
				t.Errorf("got %q, want %q", s, tt.want)
			}
		})
	}
}

func TestThings(t *testing.T) {
	rand.Seed(time.Now().UnixNano())
	sum := 0
	n := 100000
	for i := 0; i < n; i++ {
		atk := []int{rand.Intn(6) + 1, rand.Intn(6) + 1}
		def := []int{rand.Intn(6) + 1} //, rand.Intn(6) + 1}
		sort.Ints(atk)
		sort.Ints(def)
		if def[0] < atk[0] { // && def[1] < atk[1] {
			sum++
		}

	}
	fmt.Println(float64(sum) / float64(n))
}
