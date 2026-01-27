# Documentation — Markowitz Portfolio Optimizer (V10)

> **Objectif** : Construire, analyser, comparer et backtester des portefeuilles optimisés selon la théorie moderne du portefeuille (Markowitz), avec conversion FX automatique vers **CHF** et métriques de risque professionnelles.

---

## Avertissement légal

**Ceci est un outil d'analyse quantitative à but éducatif et de recherche.**

- Ce n'est **pas** un conseil en investissement
- Les performances passées ne garantissent **pas** les résultats futurs
- Les résultats dépendent fortement de la période d'analyse, des hypothèses et de la qualité des données
- Consultez un conseiller financier agréé avant toute décision d'investissement

---

## Table des matières

1. [Vue d'ensemble](#vue-densemble)
2. [Fondations mathématiques](#fondations-mathématiques)
3. [Métriques de performance](#métriques-de-performance)
4. [Robustesse statistique](#robustesse-statistique)
5. [Gestion des devises FX](#gestion-des-devises-fx)
6. [Benchmarks de référence](#benchmarks-de-référence)
7. [Projection Monte Carlo](#projection-monte-carlo)
8. [Backtest Walk-Forward](#backtest-walk-forward)
9. [Guide des paramètres](#guide-des-paramètres)
10. [Exemples pratiques](#exemples-pratiques)
11. [Interprétation des résultats](#interprétation-des-résultats)
12. [Limites et précautions](#limites-et-précautions)
13. [Glossaire](#glossaire)
14. [Références](#références)

---

## Vue d'ensemble

### Pipeline de calcul

Lorsque vous cliquez **Calculer**, l'application exécute séquentiellement :

| Étape | Description |
|:------|:------------|
| 1. PARSING | Normalisation des tickers (alias CH → .SW) |
| 2. DONNÉES | Téléchargement prix ajustés (Yahoo Finance) |
| 3. DEVISES | Détection devise + conversion FX → CHF |
| 4. RENDEMENTS | Calcul log-returns journaliers |
| 5. ESTIMATION | μ (rendements attendus) et Σ (covariance) |
| 6. ROBUSTESSE | Shrinkage Σ + ajustement μ (optionnel) |
| 7. OPTIMISATION | Frontière efficiente (Quadratic Programming) |
| 8. BENCHMARKS | Calcul S&P 500, Equal-Weight pour comparaison |
| 9. SORTIES | Graphiques, KPIs, allocations, ordres, exports |

### Onglets de l'application

| Onglet | Fonction |
|:-------|:---------|
| **Optimisation** | Construction de la frontière efficiente et sélection de portefeuille |
| **Projection** | Simulation de l'évolution future avec DCA et objectif financier |
| **Backtest** | Test out-of-sample walk-forward avec métriques professionnelles |
| **Documentation** | Ce guide |

---

## Fondations mathématiques

### Rendements logarithmiques

Soit \(P_t\) le prix ajusté d'un actif au temps \(t\) (en CHF après conversion FX).

Le **log-rendement** (ou rendement logarithmique) est défini par :

\[
r_t = \ln\left(\frac{P_t}{P_{t-1}}\right) = \ln(P_t) - \ln(P_{t-1})
\]

**Propriétés importantes :**

* Additivité temporelle : \(r_{t_0 \to t_n} = \sum_{i=1}^{n} r_{t_i}\)
* Approximation pour petits rendements : \(r_t \approx \frac{P_t - P_{t-1}}{P_{t-1}}\)
* Distribution plus proche de la normale que les rendements simples

> **Note** : Les prix "Adjusted Close" de Yahoo intègrent les dividendes et splits, approximant ainsi un **total return**.

### Estimation des paramètres

À partir d'un échantillon de \(T\) rendements journaliers, on estime :

**Rendement espéré (annualisé) :**

\[
\hat{\mu}_i = 252 \times \frac{1}{T} \sum_{t=1}^{T} r_{i,t}
\]

**Matrice de covariance (annualisée) :**

\[
\hat{\Sigma}_{ij} = 252 \times \frac{1}{T-1} \sum_{t=1}^{T} (r_{i,t} - \bar{r}_i)(r_{j,t} - \bar{r}_j)
\]

Le facteur **252** correspond au nombre approximatif de jours de bourse par an.

### Optimisation de Markowitz

#### Problème d'optimisation

Pour un rendement cible \(R^*\), on cherche les poids \(w = (w_1, \ldots, w_n)^\top\) qui minimisent la variance du portefeuille :

\[
\min_{w} \quad \frac{1}{2} w^\top \Sigma w
\]

Sous les contraintes :

\[
\sum_{i=1}^{n} w_i = 1 \quad \text{(budget)}
\]

\[
w^\top \mu = R^* \quad \text{(rendement cible)}
\]

\[
0 \leq w_i \leq w_{\max} \quad \text{(bornes)}
\]

#### Résolution

Ce problème est un **programme quadratique convexe** (QP) résolu par l'algorithme de Goldfarb-Idnani (package `quadprog`).

#### Construction de la frontière

En faisant varier \(R^*\) de \(\min(\mu_i)\) à \(\max(\mu_i)\), on obtient l'ensemble des portefeuilles optimaux formant la **frontière efficiente**.

### Portefeuilles remarquables

| Portefeuille | Définition | Caractéristique |
|:-------------|:-----------|:----------------|
| **MVP** | Minimum Variance Portfolio | Risque minimal absolu |
| **Tangent** | Maximum Sharpe Ratio | Meilleur rendement ajusté au risque |
| **Sélection** | Choisi via le slider | Compromis personnalisé risque/rendement |

Le **MVP** est défini par :

\[
w_{\text{MVP}} = \arg\min_w \sqrt{w^\top \Sigma w}
\]

Le **Tangent** est défini par :

\[
w_{\text{Tangent}} = \arg\max_w \frac{w^\top \mu - r_f}{\sqrt{w^\top \Sigma w}}
\]

### Capital Market Line

La droite reliant le taux sans risque \(r_f\) au portefeuille tangent représente les combinaisons optimales entre actif sans risque et portefeuille risqué :

\[
\mathbb{E}[R_p] = r_f + \frac{\mathbb{E}[R_T] - r_f}{\sigma_T} \times \sigma_p
\]

où \((R_T, \sigma_T)\) sont le rendement et la volatilité du portefeuille tangent.

---

## Métriques de performance

### Ratio de Sharpe

Le ratio de Sharpe mesure l'excès de rendement par unité de risque total :

\[
\text{Sharpe} = \frac{\mathbb{E}[R_p] - r_f}{\sigma_p}
\]

**Interprétation :**

| Sharpe | Qualité |
|:-------|:--------|
| < 0 | Mauvais (rendement < taux sans risque) |
| 0 – 0.5 | Faible |
| 0.5 – 1.0 | Acceptable |
| 1.0 – 2.0 | Bon |
| > 2.0 | Excellent (rare sur le long terme) |

**Limites :** Pénalise également la volatilité haussière (gains).

### Ratio de Sortino

Le ratio de Sortino ne pénalise que la **volatilité baissière** (downside risk) :

\[
\text{Sortino} = \frac{\mathbb{E}[R_p] - r_f}{\sigma_{\text{down}}}
\]

où la **volatilité downside** est :

\[
\sigma_{\text{down}} = \sqrt{\frac{1}{T} \sum_{t: r_t < 0} r_t^2}
\]

**Interprétation :**

* Sortino > Sharpe → la volatilité provient davantage des hausses (bon signe)
* Sortino < Sharpe → la volatilité provient davantage des baisses (prudence)

### Ratio de Calmar

Le ratio de Calmar mesure le rendement par rapport au pire drawdown historique :

\[
\text{Calmar} = \frac{\text{CAGR}}{\left| \text{Max Drawdown} \right|}
\]

**Interprétation :**

* Calmar > 1 : le rendement annuel dépasse la pire perte
* Utile pour évaluer la "récupération" après les crises

### Maximum Drawdown

Le drawdown mesure la perte depuis le plus haut historique :

\[
\text{Drawdown}_t = \frac{V_t - \max_{\tau \leq t} V_\tau}{\max_{\tau \leq t} V_\tau}
\]

Le **Maximum Drawdown** est :

\[
\text{MDD} = \min_t \left( \text{Drawdown}_t \right)
\]

**Exemple :** Un MDD de -25% signifie que le portefeuille a perdu 25% depuis son plus haut.

### CAGR

Le taux de croissance annuel composé (Compound Annual Growth Rate) :

\[
\text{CAGR} = \left( \frac{V_{\text{final}}}{V_{\text{initial}}} \right)^{\frac{252}{n_{\text{jours}}}} - 1
\]

### Win Rate

Proportion de jours avec un rendement positif :

\[
\text{Win Rate} = \frac{\text{Nombre de jours } r_t > 0}{\text{Nombre total de jours}}
\]

**Interprétation :**

* Win Rate > 50% : majorité de jours positifs
* Un Win Rate élevé ne garantit pas un bon rendement (dépend de l'amplitude)

### Turnover

Le turnover mesure l'activité de trading lors des rebalancements :

\[
\text{Turnover} = \frac{1}{2} \sum_{i=1}^{n} |w_{i,\text{new}} - w_{i,\text{old}}|
\]

**Impact :** Turnover élevé → coûts de transaction plus importants.

---

## Robustesse statistique

### Pourquoi la robustesse

Les estimations de \(\mu\) et \(\Sigma\) sont **bruitées** car basées sur un échantillon historique limité. L'optimisation de Markowitz amplifie ces erreurs ("error maximization").

### Shrinkage de la covariance

On combine la matrice empirique avec une matrice cible plus stable :

\[
\Sigma_{\text{shrunk}} = (1-\lambda)\hat{\Sigma} + \lambda \Sigma_{\text{target}}
\]

#### Méthodes disponibles

| Méthode | Cible | Effet |
|:--------|:------|:------|
| **Diagonal** | Matrice diagonale (variances uniquement) | Ignore les corrélations |
| **Corrélation constante** | Corrélation moyenne uniforme | Stabilise les corrélations |

Pour la méthode **corrélation constante**, la corrélation moyenne est :

\[
\bar{\rho} = \frac{2}{n(n-1)} \sum_{i<j} \rho_{ij}
\]

#### Choix du paramètre lambda

| λ | Effet |
|:--|:------|
| 0 | Matrice empirique pure (plus de bruit) |
| 0.15 – 0.30 | **Recommandé** (bon compromis) |
| 1 | Cible pure (ignore les données) |

### Robustesse des rendements attendus

Le vecteur des rendements attendus \(\mu\) est particulièrement instable.

#### Méthodes disponibles

**1. Winsorization** (coupe les extrêmes)

Les rendements extrêmes sont tronqués aux quantiles \(q_p\) et \(q_{1-p}\) (ex: 2% et 98%).

**2. Shrinkage vers la moyenne**

\[
\mu_i^{\text{shrunk}} = (1-\lambda)\hat{\mu}_i + \lambda \bar{\mu}
\]

où \(\bar{\mu} = \frac{1}{n}\sum_i \hat{\mu}_i\) est la moyenne des rendements.

**3. Shrinkage vers zéro** (conservateur)

\[
\mu_i^{\text{shrunk}} = (1-\lambda)\hat{\mu}_i
\]

#### Recommandations

| Paramètre | Valeur recommandée |
|:----------|:-------------------|
| Shrink Σ | `constcor`, λ = 0.20 |
| Shrink μ | `shrink_mean`, λ = 0.40 |

---

## Gestion des devises FX

### Pourquoi convertir en CHF

Sans conversion FX, vous mélangez des rendements dans différentes devises → la covariance et les rendements ne sont pas cohérents.

### Mécanisme de conversion

Pour un actif coté en devise C (ex: USD), on convertit le prix vers CHF :

\[
P_t^{\text{CHF}} = P_t^{C} \times \text{FX}_{C \to \text{CHF}, t}
\]

L'application utilise :

1. Taux direct : `USDCHF=X`
2. Sinon, taux inverse : `CHFUSD=X` puis 1/FX

### Impact sur les rendements

Le rendement en CHF intègre :

* Le rendement de l'actif dans sa devise native
* La variation du taux de change

\[
r_t^{\text{CHF}} \approx r_t^{\text{local}} + r_t^{\text{FX}}
\]

> **Note** : Cela ajoute une source de volatilité (risque de change) mais reflète la réalité pour un investisseur suisse.

---

## Benchmarks de référence

### Pourquoi des benchmarks

Les benchmarks permettent de contextualiser la performance de votre portefeuille optimisé et de juger si l'optimisation apporte une réelle valeur ajoutée.

### Benchmarks disponibles

| Benchmark | Description | Symbole Yahoo |
|:----------|:------------|:--------------|
| **Equal-Weight** | Répartition égale entre vos actifs | Calculé |
| **S&P 500** | 500 plus grandes entreprises US | ^GSPC |
| **MSCI World** | Actions monde développé | URTH |
| **Nasdaq 100** | 100 plus grandes tech US | ^NDX |

### Portefeuille Equal-Weight

Poids uniformes :

\[
w_i^{\text{EW}} = \frac{1}{n} \quad \forall i
\]

**Caractéristiques :**

* Simple, pas d'optimisation
* Souvent performant en pratique ("1/N puzzle")
* Sert de benchmark naïf

### Lecture du graphique

Sur la frontière efficiente :

* **Ligne bleue** : frontière efficiente (vos actifs)
* **Losanges colorés** : benchmarks
* Position relative : permet de juger si l'optimisation apporte de la valeur

---

## Projection Monte Carlo

### Objectif

Simuler l'évolution future du portefeuille en tenant compte de :

* L'incertitude sur les rendements
* Les contributions mensuelles (DCA - Dollar Cost Averaging)
* Un objectif financier à atteindre

### Modèle déterministe

Avec un rendement mensuel constant :

\[
r_m = (1 + \mu)^{1/12} - 1
\]

L'évolution avec contribution mensuelle \(c\) :

\[
V_{t+1} = V_t \times (1 + r_m) + c
\]

### Modèle Monte Carlo GBM

On suppose que les log-rendements suivent un mouvement brownien géométrique (Geometric Brownian Motion) :

\[
\ln\left(\frac{V_{t+\Delta t}}{V_t}\right) \sim \mathcal{N}\left(\left(\mu - \frac{\sigma^2}{2}\right)\Delta t, \; \sigma\sqrt{\Delta t}\right)
\]

Avec \(\Delta t = \frac{1}{12}\) (mensuel).

**Simulation :**

\[
V_{t+\Delta t} = V_t \times \exp\left(\left(\mu - \frac{\sigma^2}{2}\right)\Delta t + \sigma\sqrt{\Delta t} \times Z\right) + c
\]

où \(Z \sim \mathcal{N}(0, 1)\) est un tirage aléatoire normal.

### Probabilité d'atteindre l'objectif

Avec \(N\) simulations et un objectif \(G\) :

\[
P(\text{atteindre } G) = \frac{\text{Nombre de simulations où } V_T \geq G}{N}
\]

**Interprétation des probabilités :**

| Probabilité | Interprétation | Indicateur |
|:------------|:---------------|:-----------|
| ≥ 70% | Objectif réaliste | Vert |
| 40% – 70% | Objectif ambitieux | Orange |
| < 40% | Objectif difficile | Rouge |

### Bandes de confiance

Le graphique affiche :

* **Ligne centrale** : médiane (p50)
* **Bande foncée** : p25 – p75 (50% des scénarios)
* **Bande claire** : p10 – p90 (80% des scénarios)
* **Bande très claire** : p5 – p95 (90% des scénarios)

---

## Backtest Walk-Forward

### Principe

Le backtest **walk-forward** (ou "rolling window") simule ce qui se serait passé si vous aviez appliqué la stratégie dans le passé, avec ré-estimation périodique des paramètres.

**Schéma temporel :**

Temps ─────────────────────────────────────────────────────────►
│◄──── Train (3 ans) ────►│◄── Test ──►│
│       Estimation        │ Application │
│
│◄──── Train (3 ans) ────►│◄── Test ──►│
│       Estimation        │ Application │



### Étapes à chaque rebalancement

1. **Estimation** : calcul de μ, Σ sur la fenêtre passée
2. **Robustesse** : application du shrinkage
3. **Optimisation** : calcul de la frontière efficiente
4. **Sélection** : choix du portefeuille (tangent, MVP, ou profil)
5. **Application** : poids appliqués jusqu'au prochain rebalancement
6. **Coûts** : déduction des frais de transaction

### Coûts de transaction

À chaque rebalancement :

\[
\text{Coût} = V_t \times \text{TC} \times \text{Turnover}
\]

où TC est le coût en basis points (ex: 10 bps = 0.10%).

### Comparaison avec Equal-Weight

Le backtest affiche deux courbes :

* **Votre stratégie** : portefeuille optimisé avec rebalancement
* **Equal-Weight** : buy-and-hold avec poids égaux (sans rebalancement)

Cela permet de juger si l'optimisation apporte une valeur ajoutée après coûts.

### Graphique de drawdown

Affiche les drawdowns des deux stratégies côte à côte :

* Identifie les périodes de stress
* Compare la résilience des stratégies

---

## Guide des paramètres

### Période d'analyse

| Durée | Avantages | Inconvénients |
|:------|:----------|:--------------|
| **3 ans** | Réactif aux tendances récentes | Plus bruité, risque d'overfit |
| **5 ans** | **Bon compromis** | - |
| **10 ans** | Très stable | Peut diluer les régimes récents |

**Recommandation** : Commencez avec 5 ans, testez la sensibilité avec 3 et 10 ans.

### Taux sans risque rf

* Utilisé pour calculer le Sharpe et identifier le portefeuille tangent
* **Valeur typique** : 0.02 (2%)
* Impact : rf ↑ → Sharpe ↓ → tangent peut changer

### Poids maximum wmax

Contraint la concentration du portefeuille.

| wmax | Effet |
|:-----|:------|
| 1.0 | Pas de contrainte |
| 0.40 | Max 40% par actif |
| 0.25 | Max 25% par actif (diversifié) |

**Règle importante** : Il faut \(w_{\max} \geq \frac{1}{n}\) sinon le problème est infaisable (impossible de répartir 100% entre n actifs si chacun est limité à moins de 1/n).

### Nombre de points ngrid

* Plus de points = frontière plus fine
* **Recommandé** : 60 – 100
* Impact sur le temps de calcul

### Paramètres de backtest

| Paramètre | Recommandation | Impact |
|:----------|:---------------|:-------|
| Fenêtre train | 3 ans | Données pour estimation |
| Rebalancement | Trimestriel | Fréquence de mise à jour |
| Coûts (bps) | 10 | Réalisme des résultats |

---

## Exemples pratiques

### Exemple 1 : Construction d'un portefeuille

**Configuration :**

* Capital : 50'000 CHF
* Actifs : AAPL, MSFT, GOOGL, NOVN.SW, NESN.SW
* Période : 5 ans
* wmax : 0.35

**Résultat (exemple fictif) :**

| Portefeuille | Rendement | Volatilité | Sharpe |
|:-------------|:----------|:-----------|:-------|
| Tangent | 12.5% | 18.2% | 0.58 |
| MVP | 8.3% | 14.1% | 0.45 |
| Equal-Weight | 10.1% | 16.8% | 0.48 |

**Interprétation** : Le portefeuille tangent offre le meilleur Sharpe mais avec plus de volatilité. Le MVP est plus conservateur.

### Exemple 2 : Calcul des ordres

**Données :**

* Capital : 10'000 CHF
* Portefeuille sélectionné :
  * AAPL : 35% (prix : 180 CHF)
  * MSFT : 40% (prix : 365 CHF)
  * NOVN.SW : 25% (prix : 95 CHF)

**Calcul :**

| Actif | Poids | Montant cible | Prix CHF | Actions | Investi |
|:------|:------|:--------------|:---------|:--------|:--------|
| AAPL | 35% | 3'500 CHF | 180 | 19 | 3'420 CHF |
| MSFT | 40% | 4'000 CHF | 365 | 10 | 3'650 CHF |
| NOVN.SW | 25% | 2'500 CHF | 95 | 26 | 2'470 CHF |
| **Total** | 100% | 10'000 CHF | - | - | **9'540 CHF** |

**Cash restant** : 460 CHF

**Mode Greedy** : Essaie d'acheter des actions supplémentaires pour minimiser le cash résiduel.

### Exemple 3 : Interprétation du backtest

**Résultats (exemple fictif) :**

| Métrique | Stratégie | Equal-Weight |
|:---------|:----------|:-------------|
| CAGR | 9.2% | 8.5% |
| Volatilité | 15.3% | 17.1% |
| Sharpe | 0.47 | 0.38 |
| Sortino | 0.65 | 0.52 |
| Max Drawdown | -22% | -28% |
| Turnover | 45% | 0% |

**Analyse :**

1. La stratégie surperforme en CAGR (+0.7%)
2. Avec moins de volatilité (-1.8%)
3. Et un drawdown moindre (+6%)
4. Mais génère du turnover (45%) → coûts de transaction

**Conclusion** : La stratégie apporte de la valeur ajoutée après ajustement du risque.

### Exemple 4 : Projection avec objectif

**Configuration :**

* Capital initial : 20'000 CHF
* Contribution mensuelle : 500 CHF
* Horizon : 15 ans
* Objectif : 200'000 CHF
* Portefeuille : μ = 8%, σ = 15%

**Résultats Monte Carlo (5'000 simulations) :**

| Quantile | Valeur finale |
|:---------|:--------------|
| p10 (pessimiste) | 145'000 CHF |
| p50 (médiane) | 195'000 CHF |
| p90 (optimiste) | 285'000 CHF |

**Probabilité d'atteindre 200'000 CHF** : **47%**

**Interprétation** : L'objectif est ambitieux mais atteignable dans environ 1 cas sur 2.

---

## Interprétation des résultats

### Lecture de la frontière efficiente

**Légende du graphique :**

* Cercle rouge : Portefeuille Tangent (max Sharpe)
* Carré vert : Portefeuille MVP (min variance)
* Triangle orange : Portefeuille sélectionné
* Losanges : Benchmarks

**Zones :**

* **Au-dessus de MVP** : branche efficiente (optimal)
* **En-dessous de MVP** : branche inefficiente (même risque, moins de rendement)
* **Position relative aux benchmarks** : juge la valeur de l'optimisation

### Signaux d'alerte

| Signal | Interprétation | Action |
|:-------|:---------------|:-------|
| Sharpe négatif | Stratégie sous-performe le cash | Revoir les paramètres |
| Drawdown > 30% | Risque élevé | Réduire wmax ou augmenter shrinkage |
| Turnover > 100% | Coûts élevés | Augmenter période rebalancement |
| Frontière plate | Actifs très corrélés | Diversifier les actifs |

### Comparaison Sharpe vs Sortino

| Situation | Sharpe | Sortino | Interprétation |
|:----------|:-------|:--------|:---------------|
| A | 0.8 | 1.2 | Volatilité haussière → bon signe |
| B | 0.8 | 0.6 | Volatilité baissière → prudence |
| C | 0.8 | 0.8 | Volatilité symétrique |

---

## Limites et précautions

### Limites du modèle

| Limite | Description |
|:-------|:------------|
| **Estimation de μ** | Très instable, sensible à la période |
| **Normalité** | Les rendements ne sont pas vraiment normaux (queues épaisses) |
| **Stationnarité** | Les paramètres changent dans le temps |
| **Données historiques** | Le passé ne prédit pas le futur |

### Ce qui n'est PAS modélisé

* **Impôts** : sur les dividendes et plus-values
* **Spreads** : écart achat/vente réel
* **Liquidité** : possibilité d'exécuter les ordres
* **Slippage** : différence entre prix théorique et exécuté
* **Minimum d'achat** : certains brokers imposent des minimums

### Précautions d'usage

1. **Testez la sensibilité** : variez les paramètres et observez les changements
2. **Ne sur-optimisez pas** : trop de shrinkage ou de contraintes peut nuire
3. **Diversifiez** : n'utilisez pas seulement des actifs corrélés
4. **Rebalancez régulièrement** : les poids dérivent avec le temps
5. **Gardez du cash** : pour les opportunités et imprévus

---

## Glossaire

| Terme | Définition |
|:------|:-----------|
| **Alpha** | Rendement excédentaire par rapport au benchmark |
| **Beta** | Sensibilité au marché |
| **CAGR** | Compound Annual Growth Rate - Taux de croissance annuel composé |
| **Calmar** | Ratio CAGR / Max Drawdown |
| **CML** | Capital Market Line - Droite rf → tangent |
| **Covariance** | Mesure de co-mouvement entre actifs |
| **DCA** | Dollar Cost Averaging - Investissement régulier |
| **Drawdown** | Perte depuis le plus haut |
| **FX** | Foreign Exchange - Taux de change |
| **GBM** | Geometric Brownian Motion - Mouvement brownien géométrique |
| **MDD** | Maximum Drawdown - Pire perte depuis un sommet |
| **MVP** | Minimum Variance Portfolio |
| **QP** | Quadratic Programming - Programmation quadratique |
| **Sharpe** | Ratio rendement excédentaire / volatilité |
| **Shrinkage** | Technique de régularisation statistique |
| **Sortino** | Comme Sharpe mais volatilité downside uniquement |
| **Tangent** | Portefeuille maximisant le Sharpe |
| **Turnover** | Rotation du portefeuille |
| **Vol** | Volatilité - Écart-type des rendements |
| **Walk-forward** | Backtest avec ré-estimation périodique |
| **Win Rate** | Proportion de jours positifs |
| **Winsorization** | Troncature des valeurs extrêmes |

---

## Références

### Publications académiques

* Markowitz, H. (1952). "Portfolio Selection". *The Journal of Finance*, 7(1), 77-91.
* Sharpe, W. F. (1966). "Mutual Fund Performance". *The Journal of Business*, 39(1), 119-138.
* Sortino, F. A., & Van Der Meer, R. (1991). "Downside Risk". *The Journal of Portfolio Management*, 17(4), 27-31.
* Ledoit, O., & Wolf, M. (2004). "Honey, I Shrunk the Sample Covariance Matrix". *The Journal of Portfolio Management*, 30(4), 110-119.
* DeMiguel, V., Garlappi, L., & Uppal, R. (2009). "Optimal Versus Naive Diversification: How Inefficient is the 1/N Portfolio Strategy?". *The Review of Financial Studies*, 22(5), 1915-1953.

### Packages R utilisés

* `quadprog` : Résolution de programmes quadratiques
* `PerformanceAnalytics` : Métriques de performance
* `quantmod` : Téléchargement de données financières
* `xts` : Manipulation de séries temporelles
* `Matrix` : Opérations matricielles (nearPD)

### Sources de données

* Yahoo Finance API : Prix historiques et taux de change

---

**Version** : V10

**Dernière mise à jour** : Janvier 2025

**Application** : Markowitz Portfolio Optimizer
