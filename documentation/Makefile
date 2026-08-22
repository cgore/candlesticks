#### Copyright (C) 2026, Christopher Mark Gore,
#### Soli Deo Gloria,
#### All rights reserved.
####
#### 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
#### Web: http://cgore.com
#### Email: cgore@cgore.com
####
#### Redistribution and use in source and binary forms, with or without
#### modification, are permitted provided that the following conditions are met:
####
####     * Redistributions of source code must retain the above copyright
####       notice, this list of conditions and the following disclaimer.
####
####     * Redistributions in binary form must reproduce the above copyright
####       notice, this list of conditions and the following disclaimer in the
####       documentation and/or other materials provided with the distribution.
####
####     * Neither the name of Christopher Mark Gore nor the names of other
####       contributors may be used to endorse or promote products derived from
####       this software without specific prior written permission.
####
#### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#### AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#### IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#### ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#### LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#### CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#### SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#### INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#### CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#### ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#### POSSIBILITY OF SUCH DAMAGE.

LATEX_FILES=candlesticks.latex copyright.latex introduction.latex	\
	  schema.latex config-package.latex durations-package.latex	\
	  instrument-types-package.latex instruments-package.latex	\
	  data-source-kinds-package.latex data-sources-package.latex	\
	  exchanges-package.latex tickers-package.latex			\
	  data-retrievals-package.latex bars-package.latex		\
	  migrations-package.latex candlesticks-package.latex

all: candlesticks.dvi candlesticks.ps candlesticks.pdf

candlesticks.dvi: ${LATEX_FILES} candlesticks.bib
	latex candlesticks.latex
	bibtex candlesticks
	latex candlesticks.latex
	latex candlesticks.latex

candlesticks.ps: candlesticks.dvi
	dvips -o candlesticks.ps candlesticks.dvi

candlesticks.pdf: ${LATEX_FILES} candlesticks.bib
	pdflatex candlesticks.latex
	bibtex candlesticks
	pdflatex candlesticks.latex
	pdflatex candlesticks.latex

clean:
	rm -f candlesticks.pdf candlesticks.ps candlesticks.dvi
