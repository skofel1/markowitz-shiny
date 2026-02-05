# Documentation — Markowitz Portfolio Optimizer (V13)

> **Objectif** : Construire, analyser, comparer et backtester des portefeuilles optimisés selon la théorie moderne du portefeuille (Markowitz), avec conversion FX automatique vers **CHF**, métriques de risque professionnelles (CVaR, stress testing), et outils de suivi de portefeuille.

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
4. [Métriques de risque avancées (CVaR)](#métriques-de-risque-avancées-cvar)
5. [Stress Testing](#stress-testing)
6. [Robustesse statistique](#robustesse-statistique)
7. [Gestion des devises FX](#gestion-des-devises-fx)
8. [Benchmarks de référence](#benchmarks-de-référence)
9. [Projection Monte Carlo](#projection-monte-carlo)
10. [Backtest Walk-Forward](#backtest-walk-forward)
11. [Suivi de portefeuille (Holdings)](#suivi-de-portefeuille-holdings)
12. [Contraintes sectorielles](#contraintes-sectorielles)
13. [Guide des paramètres](#guide-des-paramètres)
14. [Exemples pratiques](#exemples-pratiques)
15. [Interprétation des résultats](#interprétation-des-résultats)
16. [Limites et précautions](#limites-et-précautions)
17. [Glossaire](#glossaire)
18. [Références](#références)
19. [Guide du débutant : Exemple pas à pas](#guide-du-débutant--exemple-pas-à-pas)

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
| **Frontière Efficiente** | Construction de la frontière efficiente et sélection de portefeuille |
| **Monte-Carlo** | Simulation de l'évolution future avec DCA et objectif financier |
| **Backtest** | Test out-of-sample walk-forward avec métriques professionnelles |
| **Stress Test** | Test de résistance aux crises historiques (GFC, COVID, etc.) |
| **Holdings** | Suivi du portefeuille réel vs cible, calcul des trades de rebalancing |
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

## Métriques de risque avancées (CVaR)

### Value at Risk (VaR)

La VaR mesure la perte maximale attendue à un niveau de confiance donné :

\[
\text{VaR}_\alpha = -\text{Quantile}_\alpha(R)
\]

**Exemple :** Une VaR(95%) de 2.5% signifie que dans 95% des cas, la perte journalière ne dépassera pas 2.5%.

**Limites de la VaR :**
* Ne dit rien sur l'ampleur des pertes au-delà du seuil
* Peut sous-estimer le risque de queue (tail risk)

### Conditional Value at Risk (CVaR / Expected Shortfall)

La CVaR (aussi appelée Expected Shortfall) mesure la perte moyenne dans les pires cas :

\[
\text{CVaR}_\alpha = \mathbb{E}[R \mid R \leq \text{VaR}_\alpha]
\]

**Interprétation :**
* CVaR(95%) = perte moyenne dans les 5% pires jours
* Toujours plus grande (en valeur absolue) que la VaR
* Métrique préférée par les régulateurs (Bâle III) et les institutionnels

**Exemple :** Si VaR(95%) = -2.5% et CVaR(95%) = -4.1%, cela signifie :
* 95% des jours, perte < 2.5%
* Dans les 5% pires jours, perte moyenne = 4.1%

| Métrique | Ce qu'elle mesure | Utilisation |
|:---------|:------------------|:------------|
| **VaR** | Seuil de perte au quantile α | Limite de risque simple |
| **CVaR** | Perte moyenne au-delà de VaR | Risque de queue (tail risk) |

---

## Stress Testing

### Objectif

Le stress testing évalue comment votre portefeuille aurait performé pendant les crises historiques majeures. C'est un complément essentiel aux métriques de risque standard.

### Scénarios disponibles

| Scénario | Période | S&P 500 | Description |
|:---------|:--------|:--------|:------------|
| **GFC 2008** | Sep-Nov 2008 | -46% | Crise financière mondiale (Lehman Brothers) |
| **COVID 2020** | Fév-Mar 2020 | -34% | Crash pandémique |
| **Euro Crisis 2011** | Juil-Sep 2011 | -18% | Crise de la dette souveraine européenne |
| **Taper Tantrum 2013** | Mai-Juin 2013 | -5% | Annonce de réduction du QE par la Fed |
| **China Deval 2015** | Août 2015 | -10% | Dévaluation du yuan chinois |
| **Vol Shock 2018** | Déc 2018 | -15% | Hausse des taux + tensions commerciales |

### Méthodologie

Le stress test utilise deux approches :

**1. Données historiques (si disponibles)**

Si vos actifs existaient pendant la période de crise, l'application calcule leur performance réelle :

\[
\text{Impact}_i = \frac{P_{\text{fin}}^i - P_{\text{début}}^i}{P_{\text{début}}^i}
\]

**2. Estimation par beta (si données manquantes)**

Pour les actifs sans historique suffisant, l'impact est estimé via leur beta au marché :

\[
\text{Impact}_i^{\text{estimé}} = \beta_i \times \text{Impact}_{\text{S\&P500}}
\]

### Impact portefeuille

L'impact total sur le portefeuille est la moyenne pondérée des impacts individuels :

\[
\text{Impact}_{\text{portefeuille}} = \sum_{i=1}^{n} w_i \times \text{Impact}_i
\]

### Interprétation

| Impact estimé | Interprétation |
|:--------------|:---------------|
| > -15% | Résistant (défensif) |
| -15% à -25% | Modéré |
| -25% à -35% | Sensible au marché |
| < -35% | Très exposé (agressif) |

**Question clé :** "Puis-je supporter cette perte sans paniquer et vendre au pire moment ?"

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

## Suivi de portefeuille (Holdings)

### Objectif

L'onglet Holdings permet de suivre votre portefeuille réel par rapport aux allocations cibles et de calculer les trades nécessaires pour rebalancer.

### Saisie des positions

Entrez vos positions actuelles dans le tableau :

| Ticker | Quantité | Prix d'achat |
|:-------|:---------|:-------------|
| AAPL | 50 | 175.00 |
| MSFT | 30 | 380.00 |
| NESN.SW | 100 | 98.50 |

L'application récupère automatiquement les prix actuels pour calculer la valeur de marché.

### Drift (dérive)

Le drift mesure l'écart entre vos poids actuels et les poids cibles :

\[
\text{Drift}_i = |w_i^{\text{actuel}} - w_i^{\text{cible}}|
\]

Le **drift maximum** est le plus grand écart parmi tous les actifs :

\[
\text{Max Drift} = \max_i(\text{Drift}_i)
\]

### Band Rebalancing (rebalancement par bandes)

Au lieu de rebalancer à dates fixes, le band rebalancing déclenche un rebalancement uniquement quand le drift dépasse un seuil :

| Seuil de drift | Stratégie |
|:---------------|:----------|
| 5% | Conservateur (rebalance souvent) |
| 10% | Modéré |
| 15% | Agressif (moins de trades) |

**Avantages :**
* Réduit les coûts de transaction
* Rebalance uniquement quand nécessaire
* Laisse les gagnants courir un peu

### Turnover Cap (plafond de turnover)

Limite le volume de transactions à chaque rebalancement :

\[
w_{\text{ajusté}} = w_{\text{actuel}} + \text{cap} \times \frac{w_{\text{cible}} - w_{\text{actuel}}}{\text{turnover}}
\]

| Turnover cap | Effet |
|:-------------|:------|
| 10% | Très progressif |
| 20% | Modéré |
| 50% | Rapide |
| 100% | Pas de limite |

**Utilisation :** Évite de tout vendre/acheter d'un coup si le portefeuille a beaucoup dérivé.

### Calcul des trades

L'application calcule automatiquement les ordres à passer :

| Ticker | Poids actuel | Poids cible | Action | Montant |
|:-------|:-------------|:------------|:-------|:--------|
| AAPL | 35% | 30% | VENDRE | -500 CHF |
| MSFT | 25% | 30% | ACHETER | +500 CHF |
| NESN.SW | 40% | 40% | - | 0 |

Les coûts de transaction estimés sont affichés pour aider à la décision.

---

## Contraintes sectorielles

### Objectif

Les contraintes sectorielles permettent de contrôler l'exposition à chaque secteur économique, évitant une sur-concentration (ex: 80% en tech).

### Détection automatique des secteurs

L'application détecte automatiquement le secteur de chaque actif via l'API Yahoo Finance :

| Secteur | Exemples |
|:--------|:---------|
| Technology | AAPL, MSFT, GOOGL |
| Healthcare | JNJ, NOVN.SW |
| Consumer Defensive | NESN.SW, PG |
| Financial Services | UBSG.SW, JPM |

### Contraintes min/max

Définissez des limites par secteur :

| Secteur | Min | Max | Effet |
|:--------|:----|:----|:------|
| Technology | 0% | 40% | Limite l'exposition tech |
| Healthcare | 10% | 30% | Garantit une allocation santé |
| Financial Services | 0% | 20% | Limite l'exposition bancaire |

### Implémentation mathématique

Les contraintes sectorielles s'ajoutent au problème d'optimisation :

\[
\text{Min}_s \leq \sum_{i \in S} w_i \leq \text{Max}_s \quad \forall \text{ secteur } s
\]

où $S$ est l'ensemble des actifs appartenant au secteur $s$.

### Cas d'usage

* **Diversification forcée** : Éviter plus de 40% dans un seul secteur
* **Allocation thématique** : Garantir au moins 20% en santé
* **Gestion du risque** : Limiter l'exposition aux secteurs cycliques

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

### Mode de sélection du portefeuille

L'application propose deux modes de sélection :

**1. Max Sharpe (défaut)**

Sélectionne automatiquement le portefeuille avec le meilleur ratio de Sharpe (tangent portfolio).

**2. Vol Target (volatilité cible)**

Sélectionne le portefeuille ayant une volatilité proche de la cible spécifiée :

| Vol cible | Profil |
|:----------|:-------|
| 8-10% | Conservateur |
| 10-12% | Modéré |
| 12-15% | Dynamique |
| 15%+ | Agressif |

**Avantage du Vol Target :** Contrôle direct du risque, indépendamment des estimations de rendement (souvent bruitées).

### Preset "Wealth Management"

Ce preset configure automatiquement des paramètres professionnels pour une gestion patrimoniale long terme :

| Paramètre | Valeur | Justification |
|:----------|:-------|:--------------|
| Période d'analyse | 10 ans | Maximum de données pour stabilité |
| Poids max | 20% | Bonne diversification |
| Shrinkage Σ | Constante Cor. (λ=0.30) | Stabilise les corrélations |
| Shrinkage μ | Shrink → Mean (λ=0.60) | Conserve un peu d'alpha |
| Mode | Vol Target 11% | Risque contrôlé |
| Rebalancing | Semestriel | Coûts réduits |
| Taux sans risque | 0.5% | Taux CHF actuel |

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
| **Band Rebalancing** | Rebalancement déclenché quand le drift dépasse un seuil |
| **Beta** | Sensibilité au marché |
| **CAGR** | Compound Annual Growth Rate - Taux de croissance annuel composé |
| **Calmar** | Ratio CAGR / Max Drawdown |
| **CML** | Capital Market Line - Droite rf → tangent |
| **Covariance** | Mesure de co-mouvement entre actifs |
| **CVaR** | Conditional VaR / Expected Shortfall - Perte moyenne dans les pires cas |
| **DCA** | Dollar Cost Averaging - Investissement régulier |
| **Drawdown** | Perte depuis le plus haut |
| **Drift** | Écart entre poids actuel et poids cible |
| **FX** | Foreign Exchange - Taux de change |
| **GBM** | Geometric Brownian Motion - Mouvement brownien géométrique |
| **Holdings** | Positions réelles détenues en portefeuille |
| **MDD** | Maximum Drawdown - Pire perte depuis un sommet |
| **MVP** | Minimum Variance Portfolio |
| **QP** | Quadratic Programming - Programmation quadratique |
| **Sharpe** | Ratio rendement excédentaire / volatilité |
| **Shrinkage** | Technique de régularisation statistique |
| **Sortino** | Comme Sharpe mais volatilité downside uniquement |
| **Stress Test** | Simulation de crises historiques sur le portefeuille |
| **Tangent** | Portefeuille maximisant le Sharpe |
| **Turnover** | Rotation du portefeuille |
| **Turnover Cap** | Limite sur le volume de transactions par rebalancement |
| **VaR** | Value at Risk - Perte maximale à un niveau de confiance |
| **Vol** | Volatilité - Écart-type des rendements |
| **Vol Target** | Mode de sélection par volatilité cible |
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

## Guide du débutant : Exemple pas à pas

Ce guide vous accompagne dans la création de votre premier portefeuille optimisé, en utilisant un exemple concret avec des actions réelles.

### Scénario de l'exemple

Imaginons que vous disposez de **100'000 CHF** à investir et que vous souhaitez créer un portefeuille diversifié avec les actions suivantes :

* **Tech US** : AAPL (Apple), MSFT (Microsoft), GOOGL (Google)
* **Suisse** : NESN.SW (Nestlé), NOVN.SW (Novartis), UBSG.SW (UBS)
* **Diversification** : JNJ (Johnson & Johnson), PG (Procter & Gamble)

---

### Étape 1 : Configuration des paramètres (panneau latéral gauche)

#### 1.1 Saisie des tickers

Dans le champ **"Tickers"**, entrez :

```
AAPL MSFT GOOGL NESN.SW NOVN.SW UBSG.SW JNJ PG
```

*Astuce : Pour les actions suisses, vous pouvez taper "NESTLE", "NOVARTIS", "UBS" et cocher "Utiliser aliases CH" - l'application les convertira automatiquement.*

#### 1.2 Période d'analyse

* **Date début** : Choisissez une période de 5-10 ans (ex: 2019-01-01)
* **Date fin** : Aujourd'hui

*Plus la période est longue, plus les estimations sont robustes, mais moins elles reflètent les conditions actuelles du marché.*

#### 1.3 Devise et taux sans risque

* **Devise de base** : CHF (votre devise de référence)
* **Taux sans risque** : 0.5% (taux actuel en Suisse)

#### 1.4 Contraintes de poids

* **Poids max par actif** : 25% (évite la sur-concentration)
* **Poids min** : 0% (pas de vente à découvert)

#### 1.5 Paramètres de robustesse

Pour un débutant, utilisez ces réglages recommandés :

* **Méthode Σ (covariance)** : "Constante Cor." avec λ = 0.25
* **Méthode μ (rendements)** : "Shrink → Mean" avec λ = 0.50

*Ces paramètres "shrinkent" les estimations vers des valeurs plus stables, réduisant le risque d'erreur d'estimation.*

#### 1.6 Cliquez sur "Optimiser"

Le bouton vert lance l'optimisation. Attendez quelques secondes pendant le téléchargement des données.

---

### Étape 2 : Onglet "Frontière Efficiente"

Cet onglet affiche le cœur de la théorie de Markowitz.

#### Ce que vous voyez :

* **Courbe bleue** : La frontière efficiente - tous les portefeuilles optimaux
* **Point rouge "MSR"** : Maximum Sharpe Ratio - le meilleur rendement/risque
* **Point vert "GMV"** : Global Minimum Variance - le moins risqué
* **Points gris** : Les actifs individuels
* **Ligne pointillée** : La "Capital Market Line" partant du taux sans risque

#### Comment interpréter :

1. Regardez où se situent vos actifs individuels (points gris)
2. Notez que la frontière est TOUJOURS au-dessus et à gauche des actifs individuels → La diversification améliore toujours le ratio rendement/risque
3. Le point MSR est généralement le meilleur choix pour un investisseur rationnel

#### Mode de sélection :

* **Max Sharpe (défaut)** : Sélectionne automatiquement le portefeuille MSR
* **Vol Target** : Permet de choisir une volatilité cible (ex: 12%)

#### Tableau des allocations :

Sous le graphique, vous verrez les poids recommandés. Par exemple :

| Ticker | Poids | Valeur (CHF) |
|:-------|:------|:-------------|
| AAPL | 18.5% | 18'500 |
| MSFT | 22.3% | 22'300 |
| GOOGL | 8.2% | 8'200 |
| NESN.SW | 15.1% | 15'100 |
| NOVN.SW | 12.4% | 12'400 |
| UBSG.SW | 5.0% | 5'000 |
| JNJ | 10.5% | 10'500 |
| PG | 8.0% | 8'000 |

*Note : Ces valeurs sont illustratives et changeront selon les données réelles.*

---

### Étape 3 : Onglet "Monte-Carlo"

Cet onglet simule l'évolution future de votre portefeuille.

#### Configuration :

* **Horizon** : 5 ans (projection sur 5 ans)
* **Simulations** : 1000 (nombre de scénarios simulés)
* **Capital initial** : 100'000 CHF

#### Ce que vous voyez :

* **Faisceau de courbes** : 1000 trajectoires possibles de votre portefeuille
* **Ligne centrale (médiane)** : Le scénario "moyen"
* **Bandes colorées** : Intervalles de confiance (5%-95%)

#### Comment interpréter :

Si après 5 ans vous voyez :

* **Médiane à 145'000 CHF** → Rendement attendu de +45%
* **Percentile 5% à 85'000 CHF** → Dans le pire des cas (5%), vous perdez 15%
* **Percentile 95% à 220'000 CHF** → Dans le meilleur cas, vous doublez

*Attention : Ces projections sont basées sur les données historiques et ne garantissent pas les performances futures.*

---

### Étape 4 : Onglet "Backtest"

Le backtest simule comment votre stratégie aurait performé dans le passé.

#### Configuration recommandée pour débutants :

* **Période training** : 3 ans (données pour optimiser)
* **Période test** : 2 ans (données pour valider)
* **Fréquence rebalancing** : Trimestriel (4x/an)
* **Coûts de transaction** : 10 bps (0.10%)

#### Ce que vous voyez :

1. **Graphique de performance** : Évolution de 100 CHF investis
   * Ligne bleue : Votre portefeuille optimisé
   * Ligne grise : Benchmark (équipondéré ou SPY)

2. **Tableau de métriques** :
   * **Rendement annualisé** : Performance moyenne par an
   * **Volatilité** : Risque (écart-type annualisé)
   * **Sharpe Ratio** : Rendement ajusté du risque (>1 = bon)
   * **Max Drawdown** : Pire perte depuis un pic (-20% = a perdu 20% max)
   * **CVaR 95%** : Perte moyenne dans les 5% pires jours

#### Comment interpréter :

Comparez votre portefeuille au benchmark :

* Si Sharpe(portefeuille) > Sharpe(benchmark) → L'optimisation ajoute de la valeur
* Si Max Drawdown est similaire → Le risque est comparable
* Si la courbe bleue est au-dessus de la grise → Surperformance historique

---

### Étape 5 : Onglet "Stress Test"

Testez la résistance de votre portefeuille face aux crises historiques.

#### Scénarios disponibles :

| Crise | Période | Impact typique |
|:------|:--------|:---------------|
| GFC 2008 | Sep-Nov 2008 | -46% à -50% |
| COVID 2020 | Fév-Mar 2020 | -34% à -35% |
| Euro Crisis 2011 | Juil-Sep 2011 | -18% à -20% |
| Taper Tantrum 2013 | Mai-Juin 2013 | -5% à -7% |
| China Deval 2015 | Août 2015 | -10% à -12% |
| Vol Shock 2018 | Déc 2018 | -15% à -20% |

#### Comment utiliser :

1. Sélectionnez un scénario (ex: "COVID 2020")
2. Cliquez "Lancer Stress Test"
3. Observez l'impact estimé sur votre portefeuille

#### Interprétation :

Si votre portefeuille affiche -28% pendant COVID contre -34% pour le S&P500 :

* Votre diversification a réduit les pertes de 6 points
* Demandez-vous : "Puis-je supporter une perte de -28% sans paniquer ?"

---

### Étape 6 : Onglet "Holdings" (suivi du portefeuille)

Une fois que vous avez investi, cet onglet vous aide à suivre votre portefeuille réel.

#### Saisie de vos positions actuelles :

Dans le tableau "Holdings actuels", entrez vos positions réelles :

| Ticker | Quantité | Prix d'achat |
|:-------|:---------|:-------------|
| AAPL | 100 | 175.50 |
| MSFT | 55 | 380.00 |
| NESN.SW | 150 | 98.50 |
| ... | ... | ... |

#### Ce que vous voyez :

* **Poids actuels vs cibles** : Graphique comparatif
* **Drift par actif** : Écart entre position actuelle et cible
* **Trades recommandés** : Achats/ventes pour revenir à la cible

#### Rebalancing :

Configurez vos règles de rebalancing :

* **Seuil de drift** : 5% (rebalance si un actif dévie de plus de 5%)
* **Turnover cap** : 20% (limite les transactions à 20% du portefeuille)

Si le drift dépasse le seuil, l'application vous suggère les trades à exécuter.

---

### Étape 7 : Utiliser les presets

Pour simplifier, utilisez les profils prédéfinis :

#### Bouton "Wealth Management" (recommandé)

Ce preset applique automatiquement des paramètres professionnels :

* Période d'analyse : 10 ans
* Poids max : 20%
* Shrinkage covariance : Constante Cor. (λ=0.30)
* Shrinkage rendements : Shrink → Mean (λ=0.60)
* Mode : Vol Target à 11%
* Rebalancing : Semestriel

*Ce profil est conçu pour une gestion patrimoniale long terme, privilégiant la robustesse à la maximisation du rendement.*

---

### Résumé du workflow

1. **Définir** : Entrez vos tickers et paramètres (ou utilisez un preset)
2. **Optimiser** : Cliquez le bouton vert
3. **Analyser** : Étudiez la frontière efficiente et les allocations
4. **Projeter** : Utilisez Monte-Carlo pour voir les scénarios futurs
5. **Valider** : Lancez un backtest pour vérifier la stratégie
6. **Stress-tester** : Vérifiez la résistance aux crises
7. **Implémenter** : Achetez selon les poids recommandés
8. **Suivre** : Utilisez Holdings pour monitorer et rebalancer

---

### Erreurs courantes à éviter

| Erreur | Pourquoi c'est problématique | Solution |
|:-------|:-----------------------------|:---------|
| Période trop courte | Estimations instables, biais récent | Utilisez minimum 5 ans de données |
| Pas de shrinkage | Sur-ajustement aux données passées | Activez shrinkage Σ et μ |
| Poids max trop élevé | Sur-concentration risquée | Limitez à 20-25% par actif |
| Ignorer les coûts | Performance irréaliste | Incluez 10-20 bps de coûts |
| Rebalancer trop souvent | Coûts de transaction élevés | Trimestriel ou semestriel suffit |
| Ignorer le stress test | Surpris par les crises | Vérifiez votre tolérance aux pertes |

---

### Conseils finaux

* **Commencez simple** : 6-10 actifs suffisent pour une bonne diversification
* **Soyez patient** : L'optimisation Markowitz est une stratégie long terme
* **Restez discipliné** : Suivez les règles de rebalancing sans émotion
* **Diversifiez les sources** : Mélangez régions, secteurs et classes d'actifs
* **Révisez annuellement** : Vos objectifs et tolérance au risque peuvent changer

---

**Version** : V13

**Dernière mise à jour** : Février 2025

**Application** : Markowitz Portfolio Optimizer
