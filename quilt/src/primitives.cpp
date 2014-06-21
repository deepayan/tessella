#include <QGraphicsScene>
#include <QColor>
#include <QPen>
#include <QBrush>

#include <qtbase.h>

extern "C" {
SEXP quilt_tpolygon(SEXP scenep, SEXP xp, SEXP yp, 
		    SEXP npolyp, SEXP istartp, SEXP iendp,
		    SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp);
SEXP quilt_trect(SEXP scenep, SEXP xp, SEXP yp, SEXP wp, SEXP hp, 
		 SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp);
}


#define get_elt(x, i, n) x[(i) % (n)]
#define get_index(i, n) (i) % (n)
#define make_qcolor(x, i) QColor(x[(i)], x[(i)+1], x[(i)+2], x[(i)+3])

Qt::PenStyle lty2style(int lty) 
{
    switch (lty) {
    case 1: return Qt::SolidLine;
    case 2: return Qt::DashLine;
    case 3: return Qt::DotLine;
    case 4: return Qt::DashDotLine;
    case 5: return Qt::DashDotDotLine;
    default: return Qt::SolidLine;
    }
}


SEXP
quilt_tpolygon(SEXP scenep, SEXP xp, SEXP yp, 
	       SEXP npolyp, SEXP istartp, SEXP iendp,
	       SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp)
{
    QGraphicsScene *scene = unwrapSmoke(scenep, QGraphicsScene);
    QPolygonF polygon;
    int i, j, k, 
	npoly = asInteger(npolyp),
	*istart = INTEGER(istartp), // NOTE: R indexing, 1-based
	*iend = INTEGER(iendp),     // NOTE: R indexing, 1-based
	*col = INTEGER(colp),       // 4 x ncol matrix
	*fill = INTEGER(fillp),     // 4 x nfill matrix
	*lty = INTEGER(ltyp),
	ncol = length(colp) / 4,
	nfill = length(fillp) / 4,
	nlty = length(ltyp),
	nlwd = length(lwdp);
    double 
	*x = REAL(xp), 
	*y = REAL(yp),
	*lwd = REAL(lwdp);
    QPen pen;
    QBrush brush;
    QColor qstroke, qfill;
    if (npolyp <= 0) return R_NilValue;
    bool vpar = (ncol > 1 || nfill > 1 || nlty > 1 || nlwd > 1);
    if (!vpar) {       // no need to update pen/brush everytime
	qstroke = make_qcolor(col, 0);
	qfill = make_qcolor(fill, 0);
	if (qstroke.alpha() > 0) {
	    pen.setStyle(lty2style(lty[0]));
	    pen.setColor(qstroke);
	    pen.setWidthF(lwd[0]);
	}
	else pen.setStyle(Qt::NoPen);
	if (qfill.alpha() > 0) {
	    brush.setColor(qfill);
	    brush.setStyle(Qt::SolidPattern);
	}
	else brush.setStyle(Qt::NoBrush);
    }
    for (k = 0; k < npoly; k++) {
	polygon.clear();
	if (iend[k] - istart[k] > 0) {
	    if (vpar) {
		qstroke = make_qcolor(col, 4 * (get_index(k, ncol)));
		qfill = make_qcolor(fill, 4 * (get_index(k, nfill)));
		if (qstroke.alpha() > 0) {
		    pen.setStyle(lty2style(get_elt(lty, k, nlty)));
		    pen.setColor(qstroke);
		    pen.setWidthF(get_elt(lwd, k, nlwd));
		}
		else pen.setStyle(Qt::NoPen);
		if (qfill.alpha() > 0) {
		    brush.setColor(qfill);
		    brush.setStyle(Qt::SolidPattern);
		}
		else brush.setStyle(Qt::NoBrush);
	    }
	    j = istart[k] - 1;
	    for (i = j; i < iend[k]; i++) {
		polygon <<  QPointF(x[i], y[i]);
	    }
	    // polygon <<  QPointF(x[j], y[j]);
	    scene->addPolygon(polygon, pen, brush);
	}
    }
    return R_NilValue;
}


SEXP
quilt_trect(SEXP scenep, SEXP xp, SEXP yp, SEXP wp, SEXP hp, 
	    SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp)
{
    QGraphicsScene *scene = unwrapSmoke(scenep, QGraphicsScene);
    QRectF rect;
    int k, 
	n = length(xp),
	*col = INTEGER(colp),       // 4 x ncol matrix
	*fill = INTEGER(fillp),     // 4 x nfill matrix
	*lty = INTEGER(ltyp),
	ncol = length(colp) / 4,
	nfill = length(fillp) / 4,
	nlty = length(ltyp),
	nlwd = length(lwdp);
    double 
	*x = REAL(xp), 
	*y = REAL(yp),
	*w = REAL(wp), 
	*h = REAL(hp),
	*lwd = REAL(lwdp);
    QPen pen;
    QBrush brush;
    QColor qstroke, qfill;
    if (n <= 0) return R_NilValue;
    bool vpar = (ncol > 1 || nfill > 1 || nlty > 1 || nlwd > 1);
    if (!vpar) {       // no need to update pen/brush everytime
	qstroke = make_qcolor(col, 0);
	qfill = make_qcolor(fill, 0);
	if (qstroke.alpha() > 0) {
	    pen.setStyle(lty2style(lty[0]));
	    pen.setColor(qstroke);
	    pen.setWidthF(lwd[0]);
	}
	else pen.setStyle(Qt::NoPen);
	if (qfill.alpha() > 0) {
	    brush.setColor(qfill);
	    brush.setStyle(Qt::SolidPattern);
	}
	else brush.setStyle(Qt::NoBrush);
    }
    for (k = 0; k < n; k++) {
	if (! (ISNAN(x[k]) || ISNAN(y[k]) || ISNAN(w[k]) || ISNAN(h[k]))) {
	    if (vpar) {
		qstroke = make_qcolor(col, 4 * (get_index(k, ncol)));
		qfill = make_qcolor(fill, 4 * (get_index(k, nfill)));
		if (qstroke.alpha() > 0) {
		    pen.setStyle(lty2style(get_elt(lty, k, nlty)));
		    pen.setColor(qstroke);
		    pen.setWidthF(get_elt(lwd, k, nlwd));
		}
		else pen.setStyle(Qt::NoPen);
		if (qfill.alpha() > 0) {
		    brush.setColor(qfill);
		    brush.setStyle(Qt::SolidPattern);
		}
		else brush.setStyle(Qt::NoBrush);
	    }
	    // FIXME: compare if we make new rect everytime
	    rect.setRect(x[k], y[k], w[k], h[k]);
	    scene->addRect(rect, pen, brush);
	    // scene->addRect(QRectF(x[k], y[k], w[k], h[k]), pen, brush);
	}
    }
    return R_NilValue;
}


