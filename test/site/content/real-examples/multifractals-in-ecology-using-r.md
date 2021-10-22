+++
title = "Multifractals in ecology using R"
author = ["Leonardo A. Saravia"]
date = 2017-11-28T10:48:00-05:00
tags = ["real-examples", "math", "equations"]
draft = false
source = "https://github.com/lsaravia/MultifractalsInR/blob/master/Curso3.md"
+++

Disclaimer
: This post is from the [link](https://github.com/lsaravia/MultifractalsInR/blob/master/Curso3.md) posted by GitHub user
    [**lsaravia**](https://github.com/lsaravia) in [this comment](https://github.com/gohugoio/hugo/issues/234#issuecomment-347532166). All credit for this post
    goes to the original author.

---

{{< figure src="/images/MultifractalsInR/fractal-ice.jpg" >}}


## Multifractals {#multifractals}

-   Many natural systems cannot be characterized by a single number such
    as the fractal dimension. Instead an infinite spectrum of dimensions
    must be introduced.

    {{< figure src="/images/MultifractalsInR/C3_Clouds.png" >}}


## Multifractal definition {#multifractal-definition}

-   Consider a given object \\(\Omega\\), its multifractal nature is
    practically determined by covering the system with a set of boxes
    \\(\\{B\_i( r)\\}\\) with \\((i=1,..., N( r))\\) of side length \\(r\\)
-   These boxes are nonoverlaping and such that

    \\[\Omega = \bigcup\_{i=1}^{N( r)} B\_i( r)\\]

    This is the box-counting method but now a measure \\(\mu(B\_n)\\) for each
    box is computed. This measure corresponds to the total population or
    biomass contained in \\(B\_n\\), in general will scale as:

    \\[\mu(B\_n) \propto r^\alpha\\]


## Box counting {#box-counting}

{{< figure src="/images/MultifractalsInR/C3_BoxCounting.png" >}}


## The generalized dimensions {#the-generalized-dimensions}

-   The fractal dimension \\(D\\) already defined is actually one of an
    infinite spectrum of so-called correlation dimension of order \\(q\\) or
    also called Renyi entropies.

    \\[D\_q = \lim\_{r \to 0} \frac{1}{q-1}\frac{log \left[ \sum\_{i=1}^{N( r)}p\_i^q \right]}{\log r}\\]

    where \\(p\_i=\mu(B\_i)\\) and a normalization is assumed:

    \\[\sum\_{i=1}^{N( r)}p\_i=1\\]

-   For \\(q=0\\) we have the familiar definition of fractal dimension. To see
    this we replace \\(q=0\\)

    \\[D\_0 = -\lim\_{r \to 0}\frac{N( r)}{\log r}\\]


## Generalized dimensions 1 {#generalized-dimensions-1}

-   It can be shown that the inequality \\(D\_q' \leq D\_q\\) holds for
    \\(q' \geq q\\)
-   The sum

    \\[M\_q( r) = \sum\_{i=1}^{N( r)}[\mu(B\_i( r))]^q = \sum\_{i=1}^{N( r)}p\_i^q\\]

    is the so-called moment or partition function of order \\(q\\).
-   Varying q allows to measure the non-homogeneity of the pattern. The
    moments with larger \\(q\\) will be dominated by the densest boxes. For
    \\(q<0\\) will come from small \\(p\_i\\)'s.
-   Alternatively we can think that for \\(q>0\\), \\(D\_q\\) reflects the scaling
    of the large fluctuations and strong singularities. In contrast, for
    \\(q<0\\), \\(D\_q\\) reflects the scaling of the small fluctuations and weak
    singularities.


## Exercise {#exercise}

-   Calculate the partition function for the center and lower images of
    the figure:

    {{< figure src="/images/MultifractalsInR/C3_BoxCounting.png" >}}


## Two important dimensions {#two-important-dimensions}

-   Two particular cases are \\(q=1\\) and \\(q=2\\). The dimension for \\(q=1\\) is
    the Shannon entropy or also called by ecologist the Shannon's index of
    diversity.

    \\[D\_1 = -\lim\_{r \to 0}\sum\_{i=1}^{N( r)} p\_i \log p\_i\\]

    and the second is the so-called correlation dimension:

    \\[D\_2 = -\lim\_{r \to 0} \frac{\log \left[ \sum\_{i=1}^{N( r)} p\_i^2 \right]}{\log r} \\]

    the numerator is the log of the Simpson index.


## Application {#application}

-   Salinity stress in the cladoceran Daphniopsis Australis. Behavioral
    experiments were conducted on individual males, and their successive
    displacements analyzed using the generalized dimension function \\(D\_q\\)
    and the mass exponent function \\(\tau\_q\\)

    {{< figure src="/images/MultifractalsInR/C3_Cladoceran.png" >}}

    both functions indicate that the successive displacements of male D.
    australis have weaker multifractal properties. This is consistent with
    and generalizes previous results showing a decrease in the complexity
    of behavioral sequences under stressful conditions for a range of
    organisms.
-   A shift between multifractal and fractal properties or a change in
    multifractal properties, in animal behavior is then suggested as a
    potential diagnostic tool to assess animal stress levels and health.


## Mass exponent and Hurst exponent {#mass-exponent-and-hurst-exponent}

-   The same information contained in the generalized dimensions can be
    expressed using mass exponents:

    \\[M\_q( r) \propto r^{-\tau\_q}\\]

    This is the scaling of the partition function. For monofractals
    \\(\tau\_q\\) is linear and related to the Hurst exponent:

    \\[\tau\_q = q H - 1\\]

    For multifractals we have

    \\[\tau\_q = (q -1) D\_q\\]

    Note that for \\(q=0\\), \\(D\_q = \tau\_q\\) and for \\(q=1\\), \\(\tau\_q=0\\)


## Paper {#paper}

1.  Kellner JR, Asner GP (2009) Convergent structural responses of
    tropical forests to diverse disturbance regimes. Ecology Letters 12:
    887--897. https://doi.org/10.1111/j.1461-0248.2009.01345.x.
