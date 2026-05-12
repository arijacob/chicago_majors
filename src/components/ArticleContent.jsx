import React, { useState, useEffect } from 'react';
import {  ScrollContainer, IntroAnimation, AriChartDemo, Substitution, Penn, Outcomes, SocialSciencesHumanities} from "./Interactives.jsx"
import {
    headlinelg, subhead, byline,
    section1,
    section2, section2Header,
    section3, section3Header,
    section4, section4Header,
    section5, section5Header,
    section6, section6Header,
    editorsNote
} from '../../public/content.js';

const ArticleSection = ({ header, paragraphs }) => (
    <section className="max-w-2xl mx-auto px-4 my-12">
        {header && (
            <h2 className="text-3xl font-bold mb-6 font-serif">
                {header}
            </h2>
        )}
        {paragraphs.map((paragraph, i) => (
            <p
                key={i}
                className="text-lg leading-relaxed mb-5 font-serif"
                dangerouslySetInnerHTML={{ __html: paragraph }}
            />
        ))}
    </section>
);

export default function ArticleContent({ windowHeight, windowWidth }) {
    const spacing = .5;
    const height = windowHeight * spacing;

    const [scrollY, setScrollY] = useState(() => {
        const saved = localStorage.getItem('scrollY');
        return saved !== null ? parseInt(saved) : 0;
    });

    const [currentStepIndex, setCurrentStepIndex] = useState(() => {
        const saved = localStorage.getItem('currentStepIndex');
        return saved !== null && scrollY > 2000 ? parseInt(saved) : 0;
    });

    useEffect(() => {
        localStorage.setItem('currentStepIndex', currentStepIndex.toString());
    }, [currentStepIndex]);

    useEffect(() => {
        localStorage.setItem('scrollY', scrollY.toString());
    }, [scrollY]);

    useEffect(() => {
        const handleScroll = () => {
            setScrollY(window.scrollY);
        };
        window.addEventListener('scroll', handleScroll);
        return () => window.removeEventListener('scroll', handleScroll);
    }, []);


    useEffect(() => {
        if (scrollY <= 100) {
            setCurrentStepIndex(0);
        }
    }, [scrollY]);

    const onStepEnter = ({ data }) => {
        setCurrentStepIndex(data);
    };

    const onStepExit = ({ data, direction }) => {
        if (direction === 'up') {
            setCurrentStepIndex(data - 1);
        } else if (direction === 'down') {
            setCurrentStepIndex(data);
        }
    };

    const baseProps = {
        currentStepIndex,
        windowWidth,
        onStepEnter,
        onStepExit,
        height,
    };

    return (
        <div className="[overflow-x:clip]">


            {/* Scrollytelling chart opener */}
            <IntroAnimation />

            {/* Section 1: opening prose */}
            <ArticleSection paragraphs={section1.slice(0, 7)} />

            {/* Penn comparison chart (mentioned at end of section 1) */}
            <div className="flex justify-center my-16">
                <Penn />
            </div>

             <ArticleSection paragraphs={section1.slice(7)} />

            {/* Section 2: fiefdom in Saieh */}
            <ArticleSection  paragraphs={section2.slice(0,9)} />

            {/* Substitution chart (math vs public policy) */}
            <div className="flex justify-center my-16">
                <Substitution />
            </div>

            <ArticleSection paragraphs={section2.slice(9, 17)} />

            {/* Outcomes chart (finance/business) */}
            <div className="flex justify-center my-16">
                <Outcomes />
            </div>

            <ArticleSection paragraphs={section2.slice(17)} />

            {/* Section 3: history of UChicago */}
            <ArticleSection paragraphs={section3} />

            {/* Section 4: Newman & humanities decline */}
            <ArticleSection paragraphs={section4.slice(0,2)} />

            {/* English/PoliSci chart */}
            <div className="flex justify-center my-16">
                <AriChartDemo />
            </div>

            <ArticleSection paragraphs={section4.slice(2, 8)} />

            {/* Share of students vs share of degrees chart */}
            <div className="flex justify-center my-16">
                <SocialSciencesHumanities />
            </div>

            <ArticleSection paragraphs={section4.slice(8)} />

            {/* Section 5: humanities response */}
            <ArticleSection paragraphs={section5} />

            {/* Section 6: conclusion */}
            <ArticleSection paragraphs={section6} />

            {/* Editor's note */}
            <section className="max-w-2xl mx-auto px-4 my-12 text-sm">
                <p dangerouslySetInnerHTML={{ __html: editorsNote }} />
            </section>
        </div>
    );
}
